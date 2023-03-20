{-# LANGUAGE OverloadedStrings #-}

import CommandArgs (Config (..), ConfigOrVersion (..), Input (..), OutputFormat (..))
import qualified Data.List as List
import Data.List.Extra (nubSort)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified GenBashCompletions as GenBash
import qualified GenFishCompletions as GenFish
import qualified GenZshCompletions as GenZsh
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import HelpParser (optName, optPart)
import Io (getInputContent, pageToCommandSimple, run, toScript)
import qualified Layout
import qualified Postprocess
import Subcommand (firstTwoWordsLoc)
import System.Environment (setEnv)
import System.FilePath (takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Printf (printf)
import Type (Opt (..), OptName (..), OptNameType (..))
import Utils (convertTabsToSpaces, getMostFrequent, isUsageBlock, splitsAt, toContiguousChunks, toRanges)

main :: IO ()
main = do
  setEnv "COLUMNS" "1000"
  defaultMain $
    testGroup
      "Tests"
      [ optNameTests,
        propertyTests,
        outdatedTests,
        devTests,
        optPartTests,
        unsupportedCases,
        miscTests,
        shellCompTests,
        shellCompGoldenTests,
        integratedGoldenTestsCommandInput,
        integratedGoldenTestsFileInput,
        integratedGoldenTestsJsonInput,
        layoutTests,
        parseUsageTests
      ]

outdatedTests :: TestTree
outdatedTests =
  testGroup
    "\n ============= unit tests against parse  ============= "
    [ test_parser "--help   baba keke" (["--help"], "", "baba keke"),
      test_parser "-h,--help   baba keke" (["-h", "--help"], "", "baba keke"),
      test_parser "--           baba" (["--"], "", "baba"),
      test_parser "-h, --help   baba" (["-h", "--help"], "", "baba"),
      test_parser "-o ARG   baba" (["-o"], "ARG", "baba"),
      test_parser "-o,--out ARG   baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o,--out=ARG   baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o,--out ARG\n   baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o,--out ARG:\n   baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o <ARG1, ARG2>   baba" (["-o"], "<ARG1, ARG2>", "baba"),
      test_parser "-o <ARG1>,<ARG2>   baba" (["-o"], "<ARG1>,<ARG2>", "baba"),
      test_parser "-o arg --output arg    baba" (["-o", "--output"], "arg", "baba"),
      test_parser "-o=ARG   baba" (["-o"], "ARG", "baba"),
      test_parseMany
        "--out=ARG[,ARG2] baba"
        [(["--out"], "ARG", "baba"), (["--out"], "ARG,ARG2", "baba")],
      test_parser "--out, -o ARG    baba" (["--out", "-o"], "ARG", "baba"),
      test_parser "--out=ARG1:ARG2\n baba" (["--out"], "ARG1:ARG2", "baba"),
      test_parser
        "-o FILE --out=FILE    without comma, with = sign"
        (["-o", "--out"], "FILE", "without comma, with = sign"),
      test_parser
        "-i <file>, --input <file>   with comma, without = sign"
        (["-i", "--input"], "<file>", "with comma, without = sign"),
      test_parser
        "-o<ARG1> <ARG2>   baba"
        (["-o"], "<ARG1> <ARG2>", "baba"),
      test_parser
        "-o{arg} --output{arg}    baba"
        (["-o", "--output"], "{arg}", "baba"),
      -- examples in the wild
      test_parser
        "  -E, --show-ends          display $ at end of each line"
        (["-E", "--show-ends"], "", "display $ at end of each line"),
      test_parser
        " -h --help       Print this help file and exit"
        (["-h", "--help"], "", "Print this help file and exit"),
      test_parser
        "--min_length    Sets an artificial lower limit"
        (["--min_length"], "", "Sets an artificial lower limit"),
      ---- gzip ----
      test_parser
        "-S .suf --suffix .suf\n       When compressing, use suffix .suf instead of .gz."
        (["-S", "--suffix"], ".suf", "When compressing, use suffix .suf instead of .gz."),
      ---- tar ----
      test_parser
        "-A, --catenate, --concatenate   append tar files to an archive"
        (["-A", "--catenate", "--concatenate"], "", "append tar files to an archive"),
      --
      test_parser
        "-w, --line-width int                  line width"
        (["-w", "--line-width"], "int", "line width"),
      test_parser
        "--stderr=e|a|c           change stderr output mode"
        (["--stderr"], "e|a|c", "change stderr output mode"),
      ---- rsync ----
      test_parser
        "--remote-option=OPT, -M  send OPTION to the remote side only"
        (["--remote-option", "-M"], "OPT", "send OPTION to the remote side only"),
      --
      ---- conda ----
      test_parser
        " -p PATH, --prefix PATH\n           Full path to environment location"
        (["-p", "--prefix"], "PATH", "Full path to environment location"),
      ---- minimap2 ----
      test_parseMany
        "--cs[=STR]   output the cs tag; STR is 'short' (if absent) or 'long' [none]"
        [(["--cs"], "[=STR]", "output the cs tag; STR is 'short' (if absent) or 'long' [none]")],
      test_parseMany
        " -O INT[,INT] gap open penalty [4,24]"
        [ (["-O"], "INT", "gap open penalty [4,24]"),
          (["-O"], "INT,INT", "gap open penalty [4,24]")
        ],
      ---- stack ----
      test_parseMany
        "--[no-]dump-logs         Enable/disable dump the build output logs"
        [ (["--dump-logs"], "", "Enable/disable dump the build output logs"),
          (["--no-dump-logs"], "", "Enable/disable dump the build output logs")
        ],
      ---- 7z ----
      test_parser
        "-o{Directory}\n       Set Output directory"
        (["-o"], "{Directory}", "Set Output directory"),
      test_parseMany
        "-si[{name}] : read data from stdin"
        [(["-si"], "", "read data from stdin"), (["-si"], "{name}", "read data from stdin")],
      ---- youtube-dl ---
      test_parser
        "    -4, --force-ipv4                     Make all connections via IPv4"
        (["-4", "--force-ipv4"], "", "Make all connections via IPv4"),
      ---- stack ----
      test_parser
        "--docker*                Run 'stack --docker-help' for details"
        (["--docker"], "", "Run 'stack --docker-help' for details"),
      test_parser
        "    -u, --username USERNAME              Login with this account ID"
        (["-u", "--username"], "USERNAME", "Login with this account ID"),
      ---- blastn ----
      test_parser
        " -template_type <String, `coding', `coding_and_optimal', `optimal'>\n    Discontiguous MegaBLAST template type"
        (["-template_type"], "<String, `coding', `coding_and_optimal', `optimal'>", "Discontiguous MegaBLAST template type"),
      ---- readseq ----
      -- [FIXME] I'm unsure this behavior is okay...
      test_parseMany
        " -wid[th]=#            sequence line width"
        [(["-wid"], "[th]=#", "sequence line width")],
      test_parser
        "-extract=1000..9999  * extract all features, sequence from given base range"
        (["-extract"], "1000..9999", "* extract all features, sequence from given base range"),
      test_parseMany
        -- [FIXME] I'm unsure this behavior is okay...
        "-feat[ures]=exon,CDS...   extract sequence of selected features"
        [(["-feat"], "[ures]=exon,CDS...", "extract sequence of selected features")],
      ---- bowtie2 ----
      test_parser
        "-t/--time          print wall-clock time taken by search phases"
        (["-t", "--time"], "", "print wall-clock time taken by search phases"),
      test_parser
        " -p/--threads <int> number of alignment threads to launch (1)"
        (["-p", "--threads"], "<int>", "number of alignment threads to launch (1)"),
      test_parser
        "-F k:<int>,i:<int> query input files are continuous FASTA where reads"
        (["-F"], "k:<int>,i:<int>", "query input files are continuous FASTA where reads"),
      ---- samtools ----
      test_parser
        "-d STR:STR\n         only include reads with tag STR and associated value STR [null]"
        (["-d"], "STR:STR", "only include reads with tag STR and associated value STR [null]"),
      test_parseMany
        " --input-fmt-option OPT[=VAL]\n               Specify a single input file format option in the form"
        [(["--input-fmt-option"], "OPT[=VAL]", "Specify a single input file format option in the form")],
      test_parser
        "-@, --threads INT\n           Number of additional threads to use [0]"
        (["-@", "--threads"], "INT", "Number of additional threads to use [0]"),
      ---- bcftools ----
      test_parseMany
        "-S, --samples-file [^]<file>   file of samples to annotate (or exclude with \"^\" prefix)"
        [(["-S", "--samples-file"], "[^]<file>", "file of samples to annotate (or exclude with \"^\" prefix)")],
      ---- gridss ----
      test_parser "-o/--output: output VCF." (["-o", "--output"], "", "output VCF."),
      ---- minimap2 ----
      test_parser
        "-w INT\t Minimizer window size [2/3 of k-mer length]."
        (["-w"], "INT", "Minimizer window size [2/3 of k-mer length]."),
      ---- parallel ----
      test_parser
        "--tmpl file=repl         Copy file to repl."
        (["--tmpl"], "file=repl", "Copy file to repl."),
      test_parser
        " --tmux (Long beta testing)       Use tmux for output."
        (["--tmux"], "(Long beta testing)", "Use tmux for output."),
      ----------------
      test_parseMany
        "    --help                      baba is here\n    -i <file>, --input=<file>   keke is there"
        [(["--help"], "", "baba is here"), (["-i", "--input"], "<file>", "keke is there")],
      test_parseMany
        "--help   baba\n      !!!JUNK LINE!!!\n    -i <file>, --input=<file>   keke"
        [(["--help"], "", "baba"), (["-i", "--input"], "<file>", "keke")],
      test_parseMany
        "--he[lp]   baba\n      !!!JUNK LINE!!!\n    -i <file>, --input=<file>   keke"
        [(["--help"], "", "baba"), (["--he"], "", "baba"), (["-i", "--input"], "<file>", "keke")],
      test_parseMany
        "\n  --he[lp]\n                  baba\n          !!!JUNK LINE!!!\n          !!!ANOTHER JUNK!!!\n  -i <file>, --input=<file>   keke"
        [(["--help"], "", "baba"), (["--he"], "", "baba"), (["-i", "--input"], "<file>", "keke")],
      test_parseMany
        "       -w INT\t Minimizer window size [2/3 of k-mer length]. A minimizer is the smallest k-mer in a window of w consecutive  k-"
        [(["-w"], "INT", "Minimizer window size [2/3 of k-mer length]. A minimizer is the smallest k-mer in a window of w consecutive k-")],
      test_parseMany
        "       -w INT\t Minimizer window size [2/3 of k-mer length]. A minimizer is the smallest k-mer in a window of w consecutive  k-\n\t\t mers.\n\n       -H\t Use  homopolymer-compressed (HPC) minimizers. An HPC sequence is constructed by contracting homopolymer runs to\n\t\t a single base. An HPC minimizer is a minimizer on the HPC sequence.\n"
        [ (["-w"], "INT", "Minimizer window size [2/3 of k-mer length]. A minimizer is the smallest k-mer in a window of w consecutive k-"),
          (["-H"], "", "Use homopolymer-compressed (HPC) minimizers. An HPC sequence is constructed by contracting homopolymer runs to")
        ]
    ]

optPartTests :: TestTree
optPartTests =
  testGroup
    "\n ============= unit tests against optPart  ============= "
    [ test_optPart "--help   " (["--help"], ""),
      test_optPart "-h,--help   " (["-h", "--help"], ""),
      test_optPart "--           " (["--"], ""),
      test_optPart "-h, --help   " (["-h", "--help"], ""),
      test_optPart "-h,  --help   " (["-h", "--help"], ""),
      test_optPart "-o ARG " (["-o"], "ARG"),
      test_optPart "-o,--out ARG   " (["-o", "--out"], "ARG"),
      test_optPart "-o,--out=ARG  " (["-o", "--out"], "ARG"),
      test_optPart "-o,--out ARG\n" (["-o", "--out"], "ARG"),
      test_optPart "-o,--out ARG:ARG2\n   " (["-o", "--out"], "ARG:ARG2"),
      test_optPart "-o <ARG1, ARG2>" (["-o"], "<ARG1, ARG2>"),
      test_optPart "-o <ARG1>,<ARG2>" (["-o"], "<ARG1>,<ARG2>"),
      test_optPart "-o<ARG1> <ARG2>" (["-o"], "<ARG1> <ARG2>"),
      test_optPart "-o arg --output arg " (["-o", "--output"], "arg"),
      test_optPart "-o{arg} --output{arg} " (["-o", "--output"], "{arg}"),
      test_optPart "-o=ARG  " (["-o"], "ARG"),
      test_optPart "--out=ARG[,ARG2]  " (["--out"], "ARG[,ARG2]"),
      test_optPart "--out, -o ARG    " (["--out", "-o"], "ARG"),
      test_optPart "--out=ARG1:ARG2\n " (["--out"], "ARG1:ARG2"),
      test_optPart "--out=baba/keke/koko" (["--out"], "baba/keke/koko"),
      test_optPart "--out baba | keke | koko" (["--out"], "baba | keke | koko"),
      test_optPart "-F or --preformat" (["-F", "--preformat"], ""), -- seen in BSD man man
      test_optPart
        "-o FILE --out=FILE  "
        (["-o", "--out"], "FILE"),
      test_optPart
        "-i <file>, --input <file>  "
        (["-i", "--input"], "<file>"),
      -- examples in the wild
      test_optPart
        "  -E, --show-ends   "
        (["-E", "--show-ends"], ""),
      test_optPart
        " -h --help    "
        (["-h", "--help"], ""),
      test_optPart
        "--min_length   "
        (["--min_length"], ""),
      ---- gzip ----
      test_optPart
        "-S .suf --suffix .suf\n       "
        (["-S", "--suffix"], ".suf"),
      ---- tar ----
      test_optPart
        "-A, --catenate, --concatenate   "
        (["-A", "--catenate", "--concatenate"], ""),
      --
      test_optPart
        "-w, --line-width int             "
        (["-w", "--line-width"], "int"),
      test_optPart
        "--stderr=e|a|c         "
        (["--stderr"], "e|a|c"),
      ---- ssh ----
      test_optPart
        "-L [bind_address:]port:remote_socket"
        (["-L"], "[bind_address:]port:remote_socket"),
      ---- rsync ----
      test_optPart
        "--remote-option=OPT, -M"
        (["--remote-option", "-M"], "OPT"),
      ---- conda ----
      test_optPart
        " -p PATH, --prefix PATH"
        (["-p", "--prefix"], "PATH"),
      ---- minimap2 ----
      test_optPart
        "--cs[=STR] "
        (["--cs"], "[=STR]"),
      test_optPart
        " -O INT[,INT]"
        (["-O"], "INT[,INT]"),
      ---- 7z ----
      test_optPart
        "-o{Directory} "
        (["-o"], "{Directory}"),
      test_optPart
        "-si[{name}] "
        (["-si"], "[{name}]"),
      ---- youtube-dl ---
      test_optPart
        "    -4, --force-ipv4"
        (["-4", "--force-ipv4"], ""),
      ---- 7z --help ----
      test_optPart
        " -i[r[-|0]]{@listfile|!wildcard}"
        (["-i"], "[r[-|0]]{@listfile|!wildcard}"),
      ---- stack ----
      test_optPart
        "--docker*"
        (["--docker"], ""), -- optWord takes care of this
      test_optPart
        "    -u, --username USERNAME"
        (["-u", "--username"], "USERNAME"),
      ---- softwareupdate ----
      test_optPart
        "-d | --download"
        (["-d", "--download"], ""),
      test_optPart
        "--schedule on | off"
        (["--schedule"], "on | off"),
      ---- blastn ----
      test_optPart
        " -template_type <String, `coding', `coding_and_optimal', `optimal'> \n "
        (["-template_type"], "<String, `coding', `coding_and_optimal', `optimal'>"),
      ---- readseq ----
      test_optPart
        " -width=#            "
        (["-width"], "#"),
      test_optPart
        "-extract=1000..9999 "
        (["-extract"], "1000..9999"),
      ---- bowtie2 ----
      test_optPart "-t/--time" (["-t", "--time"], ""),
      test_optPart
        " -p/--threads <int>"
        (["-p", "--threads"], "<int>"),
      test_optPart
        "-F k:<int>,i:<int> "
        (["-F"], "k:<int>,i:<int>"),
      ---- samtools ----
      test_optPart "-d STR:STR\n        " (["-d"], "STR:STR"),
      test_optPart
        " --input-fmt-option OPT[=VAL]"
        (["--input-fmt-option"], "OPT[=VAL]"),
      test_optPart "-@, --threads INT" (["-@", "--threads"], "INT"),
      ---- bcftools ----
      test_optPart
        "-s, --samples [^]<list>"
        (["-s", "--samples"], "[^]<list>"),
      test_optPart "--ploidy <assembly>[?]" (["--ploidy"], "<assembly>[?]"),
      test_optPart " -g, --gvcf <int>,[...]" (["-g", "--gvcf"], "<int>,[...]"),
      ---- gridss ----
      test_optPart "-o/--output" (["-o", "--output"], ""),
      ---- minimap2 ----
      test_optPart "-w INT\t " (["-w"], "INT"),
      ---- bwa -----
      test_optPart "-I FLOAT[,FLOAT[,INT[,INT]]]" (["-I"], "FLOAT[,FLOAT[,INT[,INT]]]"),
      ---- parallel ----
      test_optPart
        "--tmpl file=repl   "
        (["--tmpl"], "file=repl"),
      test_optPart
        " --tmux (Long beta testing) "
        (["--tmux"], "(Long beta testing)"),
      ---- blast ----
      test_optPart
        " -window_size <Integer, >=0>\n "
        (["-window_size"], "<Integer, >=0>"),
      ---- octopus ----
      test_optPart
        " --inactive-flank-scoring arg (=1)"
        (["--inactive-flank-scoring"], "arg (=1)"),
      ---- robotframework ----
      test_optPart
        "--expandkeywords name:<pattern>|tag:<pattern> *"
        (["--expandkeywords"], "name:<pattern>|tag:<pattern> *"),
      ---- nox ----
      test_optPart
        "-s [SESSIONS ...], -e [SESSIONS ...], --sessions [SESSIONS ...], --session [SESSIONS ...]"
        (["-s", "-e", "--sessions", "--session"], "[SESSIONS ...]"),
      ---- agat ----
      test_optPart
        " -o , --output , --out or --outfile"
        (["-o", "--output", "--out", "--outfile"], ""),
      ---- bio ----
      test_optPart
        " -K '', --keep ''"
        (["-K", "--keep"], "''"),
      ---- delly ----
      test_optPart
        "-o [ --outfile ] arg (=\"sv.bcf\") "
        (["-o", "--outfile"], "arg (=\"sv.bcf\")"),
      ---- poetry ----
      test_optPart
        "-h (--help)"
        (["-h", "--help"], ""),
      ---- julia ----
      test_optPart
        "--code-coverage=@<path>"
        (["--code-coverage"], "@<path>"),
      ---- kubectl ----
      -- trailing ':' is ignored at preprocessing stage
      -- where opt+arg and description are separated.
      test_parseBlockwise
        "Options:\n\
        \    --edit=false:\n\
        \        Edit the API resource before creating\n\
        \    --field-manager='kubectl-create':\n\
        \        Name of the manager used to track field ownership.\n\
        \    --raw='':\n\
        \        Raw URI to POST to the server.  Uses the transport specified by the kubeconfig file.\n\
        \    -R, --recursive=false:\n\
        \        Process the directory used in -f, --filename recursively.\n"
        [ Opt
            [OptName "--edit" LongType]
            "false"
            "Edit the API resource before creating",
          Opt
            [OptName "--field-manager" LongType]
            "'kubectl-create'"
            "Name of the manager used to track field ownership.",
          Opt
            [OptName "--raw" LongType]
            "''"
            "Raw URI to POST to the server. Uses the transport specified by the kubeconfig file.",
          Opt
            [ OptName "-R" ShortType,
              OptName "--recursive" LongType
            ]
            "false"
            "Process the directory used in -f, --filename recursively."
        ],
      ---- micromamba ----
      test_optPart
        "--env Excludes: --system --file"
        (["--env"], "Excludes: --system --file"),
      ---- fzf ----
      test_optPart
        " --height=[~]HEIGHT[%]"
        (["--height"], "[~]HEIGHT[%]"),
      ---- hifiasm ----
      test_parseBlockwise
        "Title Blah\n\
        \   options:\n\
        \       --pri-range INT1[,INT2]\n\
        \              Min and max coverage cutoff of primary contigs.  Keep contigs with coverage in this range at p_ctg.gfa.\n\
        \              Inferred  automatically  in  default.  If INT2 is not specified, it is set to infinity.  Set -1 to disable.\n\
        \\n\
        \       --lowQ INT\n\
        \              Output contig regions with >=INT% inconsistency to the bed file with suffix lowQ.bed  [70].  Set  0  to\n\
        \              disable.\n"
        [ Opt
            [OptName "--pri-range" LongType]
            "INT1[,INT2]"
            "Min and max coverage cutoff of primary contigs. Keep contigs with coverage in this range at p_ctg.gfa. Inferred automatically in default. If INT2 is not specified, it is set to infinity. Set -1 to disable.",
          Opt
            [OptName "--lowQ" LongType]
            "INT"
            "Output contig regions with >=INT% inconsistency to the bed file with suffix lowQ.bed [70]. Set 0 to disable."
        ],
      ---- psiblast ----
      test_optPart
        " -out <File_Out, file name length < 256>"
        (["-out"], "<File_Out, file name length < 256>"),
      test_optPart
        " -out <File_Out, file name length > 0>"
        (["-out"], "<File_Out, file name length > 0>")
    ]

unsupportedCases :: TestTree
unsupportedCases =
  expectFail $
    testGroup
      "\n ============= Unsupported corner cases parse fail ============= "
      [ -- ========================================================================
        -- Just shows optPart alone cannot handle if square brackets appear in option names.
        -- In reality parseWithOptPart invokes fallback and prodesses nicely.
        test_optPart
          "-feat[ures]=exon,CDS... "
          (["--feat[ures]"], "exon,CDS..."),
        test_optPartMany "--[no-]dump-logs" [(["--dump-logs"], ""), (["--no-dump-logs"], "")],
        -- ========================================================================

        -- ================================
        -- unsupported examples starts here
        -- ================================

        ---- bcftools ----
        test_optPartMany "-h/H, --header-only/--no-header" [(["-h", "--header-only"], ""), (["-H", "--no-header"], "")],
        test_optPart " -g, --gvcf -|REF.FA  " (["-g", "--gvcf"], "-|REF.FA"),
        test_optPart "-m, --multiallelics -|+TYPE" (["-m", "--multiallelics"], "-|+TYPE"),
        test_parseBlockwise
          "  --distinctive-sites            Find sites that can distinguish between at least NUM sample pairs.\n\
          \           NUM[,MEM[,TMP]]          If the number is smaller or equal to 1, it is interpreted as the fraction of pairs."
          [ Opt
              [OptName "--distinctive-sites" LongType]
              "NUM[,MEM[,TMP]]"
              "Find sites that can distinguish between at least NUM sample pairs. If the number is smaller or equal to 1, it is interpreted as the fraction of pairs"
          ],
        ---- blastn ----
        test_optPart
          " -task <String, Permissible values: 'blastn' 'blastn-short' 'dc-megablast'\n          'megablast' 'rmblastn' >\n"
          (["-task"], "<String, Permissible values: 'blastn' 'blastn-short' 'dc-megablast' 'megablast' 'rmblastn'>"),
        ---- fastqc ----
        -- This example is unsupported due to its syntactic ambiguity in relating the first
        --  line with the rest. Guess semantics analysis is required.
        test_parser
          "    -c              Specifies a non-default file which contains the list of\n\
          \    --contaminants  contaminants to screen overrepresented sequences against.\n\
          \                    The file must contain sets of named contaminants in the\n\
          \                    form name[tab]sequence.  Lines prefixed with a hash will\n\
          \                    be ignored."
          (["-c", "--contaminants"], "", "Specifies a non-default file which contains the list of contaminants to screen overrepresented sequences against."),
        ---- hmmalign ----
        test_parseBlockwise
          "  --mapali <f>    : include alignment in file <f> (same ali that HMM came from)\n\
          \  --trim          : trim terminal tails of nonaligned residues from alignment\n\
          \  --amino         : assert <seqfile>, <hmmfile> both protein: no autodetection\n\
          \  --dna           : assert <seqfile>, <hmmfile> both DNA: no autodetection"
          [ Opt
              [OptName "--mapali" LongType]
              "<f>"
              "include alignment in file <f> (same ali that HMM came from)",
            Opt
              [OptName "--trim" LongType]
              ""
              "trim terminal tails of nonaligned residues from alignment",
            Opt
              [OptName "--amino" LongType]
              ""
              "assert <seqfile>, <hmmfile> both protein: no autodetection",
            Opt
              [OptName "--dna" LongType]
              ""
              "assert <seqfile>, <hmmfile> both DNA: no autodetection"
          ],
        ---- gem ----
        test_optPart
          "-p, --[no-]http-proxy [URL]"
          (["-p", "--no-http-proxy"], "[URL]"),
        ---- neofetch ----
        test_optPart
          "--config /path/to/config"
          (["--config"], "/path/to/config")
      ]

parseUsageTests :: TestTree
parseUsageTests =
  testGroup
    "\n ============= Test usage parser ============"
    [ test_parseUsage "  Usage:  cat [OPTION]... [FILE]...  \n" "cat [OPTION]... [FILE]...",
      test_parseUsage "SYNOPSIS\n    cat dog \nOTHER HEADER\n  baba\n" "cat dog",
      test_parseUsage "  Usage: \n    cat [OPTION]... [FILE]...\n    cat dog  \n  Other header:  keke\n\n" "cat [OPTION]... [FILE]...\ncat dog"
    ]

devTests :: TestTree
devTests =
  testGroup
    "\n ============= tests against parseMany  ============= "
    []

optNameTests :: TestTree
optNameTests =
  testGroup
    "\n ============= Test optName ============= "
    [ testCase "optName (long)" $
        readP_to_S optName "--help" @?= [(OptName "--help" LongType, "")],
      testCase "optName (short)" $
        readP_to_S optName "-o" @?= [(OptName "-o" ShortType, "")],
      testCase "optName (old)" $
        readP_to_S optName "-azvhP" @?= [(OptName "-azvhP" OldType, "")],
      testCase "optName (double dash alone)" $
        readP_to_S optName "-- " @?= [(OptName "--" DoubleDashAlone, " ")],
      testCase "optName (single dash alone)" $
        readP_to_S optName "- " @?= [(OptName "-" SingleDashAlone, " ")]
    ]

layoutTests :: TestTree
layoutTests =
  testGroup
    "Test layouts"
    [ -- convertTabsToSpaces inserts newline \n at the last.
      testCase "convertTabsToSpaces 1" $
        convertTabsToSpaces 4 "aa\tb\tccddddddddd\t" @?= "aa  b   ccddddddddd \n",
      testCase "convertTabsToSpaces 2" $
        convertTabsToSpaces 3 "\t\t\ta\tab\tabc\tkk" @?= "         a  ab abc   kk\n"
    ]

miscTests :: TestTree
miscTests =
  testGroup
    "\n ============ misc =============="
    [ testCase "firstTwoWordsLoc: empty" $
        firstTwoWordsLoc
          "  "
          @?= (-1, -1),
      testCase "firstTwoWordsLoc: single word" $
        firstTwoWordsLoc
          " hi"
          @?= (1, -1),
      testCase "firstTwoWordsLoc 0" $
        firstTwoWordsLoc
          "  stop        Stop one or more running containers   "
          @?= (2, 14),
      testCase "firstTwoWordsLoc 1" $
        firstTwoWordsLoc
          "stop        Stop one or more running containers   "
          @?= (0, 12),
      testCase "firstTwoWordsLoc 2" $
        firstTwoWordsLoc
          "   hi               there   "
          @?= (3, 20),
      testCase "getMostFrequent [1, -4, 2, 9, 1, -4, -3, 7, -4, -4, 1] == Just (-4)" $
        getMostFrequent [1 :: Int, -4, 2, 9, 1, -4, -3, 7, -4, -4, 1] @?= Just (-4 :: Int),
      testCase "toContiguousChunks [2, 3, 4, 8, 10, 11] == [[2, 3, 4], [8], [10, 11]]" $
        toContiguousChunks [2, 3, 4, 8, 10, 11] @?= [[2, 3, 4], [8], [10, 11]],
      testCase "toRanges [2, 3, 4, 8, 10, 11] == [(2, 5), (8, 9), (10, 12)]" $
        toRanges [2, 3, 4, 8, 10, 11] @?= [(2, 5), (8, 9), (10, 12)],
      testCase "splitsAt \"0123456789\" [0, 3, 5] == [\"012\", \"34\", \"56789\"]" $
        splitsAt "0123456789" [0, 3, 5] @?= ["012", "34", "56789"],
      testCase "splitsAt \"0123456789\" [0] == [\"0123456789\"]" $
        splitsAt "0123456789" [0] @?= ["0123456789"],
      testCase "splitsAt \"0123456789\" [] == [\"0123456789\"]" $
        splitsAt "0123456789" [] @?= ["0123456789"],
      testCase "truncateAfterPeriod 1" $
        GenFish.truncateAfterPeriod "hello, i.e. good bye!" @?= "hello, i.e. good bye!",
      testCase "truncateAfterPeriod 2" $
        GenFish.truncateAfterPeriod "baba. keke" @?= "baba.",
      testCase "truncateAfterPeriod 3" $
        GenFish.truncateAfterPeriod "baba... keke" @?= "baba...",
      testCase "truncateAfterPeriod 4" $
        GenFish.truncateAfterPeriod "baba .. keke" @?= "baba ..",
      testCase "fixOpt 1" $
        Postprocess.fixShortOptWithArgWithoutSpace
          (Opt [OptName "-Ttagsfile" OldType, OptName "--tag-file" LongType] "tagsfile" "Specifies a tags file...")
          @?= Opt [OptName "-T" ShortType, OptName "--tag-file" LongType] "tagsfile" "Specifies a tags file...",
      testCase "isUsageBlock" $
        Utils.isUsageBlock "Usage: rsem-bam2readdepth sorted_bam_input readdepth_output" @?= True,
      testCase "isUsageBlock" $
        Utils.isUsageBlock "SYNOPSIS\n      rsem-bam2readdepth sorted_bam_input readdepth_output" @?= True
    ]

shellCompTests :: TestTree
shellCompTests =
  testGroup
    "\n ============= Test Fish script generation ============"
    [ testCase "basic fish comp" $
        GenFish.makeFishLineOption cmd opt @?= fishExpected,
      testCase "zsh script generation" $
        GenZsh.genZshScript cmd opts @?= zshScriptExpected,
      testCase "bash script generation" $
        GenBash.genBashScript cmd opts @?= bashScriptExpected
    ]
  where
    cmd = "nanachi"
    names = [OptName "-o" ShortType, OptName "--output" LongType]
    arg = "<file>"
    desc = "Specify the filename to save"
    opt = Opt names arg desc
    fishExpected = "complete -c nanachi -s \"o\" -l \"output\" -d \"Specify the filename to save\" -r"

    names2 = [OptName "--help" LongType]
    args2 = ""
    desc2 = "Help here."
    opt2 = Opt names2 args2 desc2
    opts = [opt, opt2]
    zshScriptExpected =
      "#compdef nanachi\n\n\
      \# Auto-generated with h2o\n\n\
      \args=(\n\
      \    {-o,--output}'[Specify the filename to save]':file:_files\n\
      \    '--help[Help here.]'\n\
      \)\n\n\
      \_arguments -s $args\n"
    bashScriptExpected =
      "# Auto-generated with h2o\n\
      \\n\
      \_nanachi()\n\
      \{\n\
      \    local cur prev\n\
      \    _init_completion -s || return\n\
      \\n\
      \    case \"$prev\" in\n\
      \    esac\n\
      \\n\
      \    if ((cword == 1)); then\n\
      \        local word_list=\"  -o --output --help\"\n\
      \        COMPREPLY=( $(compgen -W \"${word_list}\" -- \"$cur\") )\n\
      \    fi\n\
      \}\n\n\
      \## -o bashdefault and -o default are fallback\n\
      \complete -o bashdefault -o default -F _nanachi nanachi\n"

shellCompGoldenTests :: TestTree
shellCompGoldenTests =
  testGroup
    "Golden Tests of shell completions"
    [ goldenVsString
        "minimap2 fish"
        "test/golden/minimap2.fish"
        (actionFish "test/golden/minimap2.txt"),
      goldenVsString
        "minimap2 zsh"
        "test/golden/minimap2.zsh"
        (actionZsh "test/golden/minimap2.txt"),
      goldenVsString
        "minimap2 bash"
        "test/golden/minimap2.sh"
        (actionBash "test/golden/minimap2.txt"),
      --------------
      goldenVsString
        "bowtie2 fish"
        "test/golden/bowtie2.fish"
        (actionFish "test/golden/bowtie2.txt")
    ]
  where
    toLazyByteString = TLE.encodeUtf8 . TL.fromStrict
    action shell x =
      toLazyByteString . toScript shell
        <$> (pageToCommandSimple (takeBaseName x) =<< getInputContent (FileInput x True))
    actionFish = action Fish
    actionZsh = action Zsh
    actionBash = action Bash

integratedGoldenTestsCommandInput :: TestTree
integratedGoldenTestsCommandInput =
  testGroup
    "Integrated tests"
    (map toTestTree commands)
  where
    commands = ["h2o", "micromamba"]
    toLazyByteString = TLE.encodeUtf8 . TL.fromStrict
    conf name = C_ (Config (CommandInput name True) Native False False False 4)
    runWithCommand name = toLazyByteString <$> run (conf name)
    toTestTree name =
      goldenVsString
        ("h2o --skip-man --command " ++ name)
        (printf "test/golden/%s.txt" name :: String)
        (runWithCommand name)

integratedGoldenTestsFileInput :: TestTree
integratedGoldenTestsFileInput =
  testGroup
    "Integrated tests"
    (map toTestTree triples)
  where
    -- [note] bcftools-mpileup is marginal and parsing give incomplete results
    --        such marginal example is a good at catching unexpected behviors.
    commandNames =
      [ "rsync",
        "grep",
        "bcftools-stats",
        "bcftools-mpileup",
        "snakemake",
        "iqtree"
      ] ::
        [String]
    inputFiles = [printf "test/golden/%s-input.txt" name | name <- commandNames]
    outputFiles = [printf "test/golden/%s.txt" name | name <- commandNames]
    triples = zip3 commandNames inputFiles outputFiles

    toLazyByteString = TLE.encodeUtf8 . TL.fromStrict
    conf filepath = C_ (Config (FileInput filepath True) Native False False False 4)
    runWithCommand filepath = toLazyByteString <$> run (conf filepath)
    toTestTree (_, inputFile, outputFile) =
      goldenVsString
        ("h2o --file " ++ inputFile)
        outputFile
        (runWithCommand inputFile)

integratedGoldenTestsJsonInput :: TestTree
integratedGoldenTestsJsonInput =
  testGroup
    "Integrated tests"
    (map toTestTree triples)
  where
    -- [note] bcftools-mpileup is marginal and parsing give incomplete results
    --        such marginal example is a good at catching unexpected behviors.
    commandNames =
      [ "h2o",
        "stack"
      ] ::
        [String]
    inputFiles = [printf "test/golden/%s.json" name | name <- commandNames]
    outputFiles = [printf "test/golden/%s-output.txt" name | name <- commandNames]
    triples = zip3 commandNames inputFiles outputFiles

    toLazyByteString = TLE.encodeUtf8 . TL.fromStrict
    conf filepath = C_ (Config (JsonInput filepath) Native False False False 4)
    runWithCommand filepath = toLazyByteString <$> run (conf filepath)
    toTestTree (_, inputFile, outputFile) =
      goldenVsString
        ("h2o --loadjson " ++ inputFile)
        outputFile
        (runWithCommand inputFile)

propertyTests :: TestTree
propertyTests =
  testGroup
    "Hedgehog tests"
    [ testProperty "optName (long)" prop_longOpt,
      testProperty "optName (short)" prop_shortOpt,
      testProperty "optName (old)" prop_oldOpt,
      testProperty "merge ranges" prop_mergeRanges
    ]

prop_longOpt :: Property
prop_longOpt =
  property $ do
    s <- forAll (Gen.string (Range.constant 1 10) Gen.alphaNum)
    let ddashed = "--" ++ s
    readP_to_S optName ddashed === [(OptName ddashed LongType, "")]

prop_shortOpt :: Property
prop_shortOpt =
  property $ do
    s <- forAll (Gen.string (Range.singleton 1) Gen.alphaNum)
    let dashed = "-" ++ s
    readP_to_S optName dashed === [(OptName dashed ShortType, "")]

prop_oldOpt :: Property
prop_oldOpt =
  property $ do
    s <- forAll (Gen.string (Range.constant 2 10) Gen.alphaNum)
    let dashed = "-" ++ s
    readP_to_S optName dashed === [(OptName dashed OldType, "")]

prop_mergeRanges :: Property
prop_mergeRanges =
  property $ do
    let num = Gen.int (Range.constant 0 200)
    xs <- forAll $ Gen.list (Range.constant 0 300) num
    ys <- forAll $ Gen.list (Range.constant 0 300) num
    let (xRanges, yRanges) = Layout.makeRangePair (nubSort xs) (nubSort ys)
    mergeRangesSlow xRanges yRanges === Layout.mergeRange xRanges yRanges

-- | O(N^2): Only for testing purposes
mergeRangesSlow :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int, Int)]
mergeRangesSlow xs ys = [(x1, x2, y1, y2) | (x1, x2) <- xs, (y1, y2) <- ys, x1 <= y1 && y1 <= x2 && x2 <= y2]

makeOpt :: [String] -> String -> String -> Opt
makeOpt names = Opt (map getOptName names)
  where
    getOptName s = case readP_to_S optName s of
      [(optname, _)] -> optname
      _ -> undefined

test_optPart :: String -> ([String], String) -> TestTree
test_optPart s (names, args) =
  testCase s $ do
    List.sort actual @?= expected
  where
    actual = map ((\(xs, y) -> (map _raw xs, y)) . fst) $ readP_to_S optPart s
    expected = [(names, args)]

test_optPartMany :: String -> [([String], String)] -> TestTree
test_optPartMany s pairs =
  testCase s $ do
    actual @?= expected
  where
    actual = List.sort $ map ((\(xs, y) -> (map show xs, y)) . fst) $ readP_to_S optPart s
    expected = List.sort pairs

test_parseBlockwise :: String -> [Opt] -> TestTree
test_parseBlockwise s opts =
  testCase s $ do
    actual @?= expected
  where
    actual = List.sort $ Layout.parseBlockwise s
    expected = List.sort opts

test_parser :: String -> ([String], String, String) -> TestTree
test_parser s (names, args, desc) =
  testCase s $ do
    actual @?= expected
  where
    actual = List.sort $ Layout.parseMany s
    expected = List.sort [makeOpt names args desc]

test_parseMany :: String -> [([String], String, String)] -> TestTree
test_parseMany s tuples =
  testCase s $ do
    actual @?= expected
  where
    actual = List.sort $ Layout.parseMany s
    expected = List.sort [makeOpt names args desc | (names, args, desc) <- tuples]

test_parseUsage :: String -> String -> TestTree
test_parseUsage s expected =
  testCase s $ do
    actual @?= expected
  where
    actual = Layout.parseUsage s
