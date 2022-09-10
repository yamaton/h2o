# Auto-generated with h2o

complete -c minimap2 -s "k" -d "Minimizer k-mer length [15]" -x
complete -c minimap2 -s "w" -d "Minimizer window size [2/3 of k-mer length]." -x
complete -c minimap2 -s "H" -d "Use homopolymer-compressed (HPC) minimizers."
complete -c minimap2 -s "I" -d "Load at most NUM target bases into RAM for indexing [4G]." -x
complete -c minimap2 -l "idx-no-seq" -d "Don't store target sequences in the index."
complete -c minimap2 -s "d" -d "Save the minimizer index of target.fa to FILE [no dump]." -r
complete -c minimap2 -l "alt" -d "List of ALT contigs [null]" -r
complete -c minimap2 -l "alt-drop" -d "Drop ALT hits by FLOAT fraction when ranking and computing mapping quality [0.15]" -x
complete -c minimap2 -s "f" -d "If fraction, ignore top FLOAT fraction of most frequent minimizers [0.0002]." -x
complete -c minimap2 -l "min-occ-floor" -d "Force minimap2 to always use k-mers occurring INT times or less [0]." -x
complete -c minimap2 -s "g" -d "Stop chain enlongation if there are no minimizers within INT-bp [10000]." -x
complete -c minimap2 -s "r" -d "Bandwidth used in chaining and DP-based alignment [500]." -x
complete -c minimap2 -s "n" -d "Discard chains consisting of <INT number of minimizers [3]" -x
complete -c minimap2 -s "m" -d "Discard chains with chaining score <INT [40]." -x
complete -c minimap2 -s "D" -d "If query sequence name/length are identical to the target name/length, ignore diagonal anchors."
complete -c minimap2 -s "P" -d "Retain all chains and don't attempt to set primary chains."
complete -c minimap2 -l "dual" -d "If no, skip query-target pairs wherein the query name is lexicographically greater than the target name [yes]" -x
complete -c minimap2 -s "X" -d "Equivalent to '-DP --dual=no --no-long-join'."
complete -c minimap2 -s "p" -d "Minimal secondary-to-primary score ratio to output secondary mappings [0.8]." -x
complete -c minimap2 -s "N" -d "Output at most INT secondary alignments [5]." -x
complete -c minimap2 -s "G" -d "Maximum gap on the reference (effective with -xsplice/--splice)." -x
complete -c minimap2 -s "F" -d "Maximum fragment length (aka insert size; effective with -xsr/--frag=yes) [800]" -x
complete -c minimap2 -s "M" -d "Mark as secondary a chain that overlaps with a better chain by FLOAT or more of the shorter chain [0.5]" -x
complete -c minimap2 -l "hard-mask-level" -d "Honor option -M and disable a heurstic to save unmapped subsequences and disables --mask-len."
complete -c minimap2 -l "mask-len" -d "Keep an alignment if dropping it leaves an unaligned region on query longer than INT [inf]." -x
complete -c minimap2 -l "max-chain-skip" -d "A heuristics that stops chaining early [25]." -x
complete -c minimap2 -l "max-chain-iter" -d "Check up to INT partial chains during chaining [5000]." -x
complete -c minimap2 -l "chain-gap-scale" -d "Scale of gap cost during chaining [1.0]" -x
complete -c minimap2 -l "no-long-join" -d "Disable the long gap patching heuristic."
complete -c minimap2 -l "lj-min-ratio" -d "Fraction of query sequence length required to bridge a long gap [0.5]." -x
complete -c minimap2 -l "splice" -d "Enable the splice alignment mode."
complete -c minimap2 -l "sr" -d "Enable short-read alignment heuristics."
complete -c minimap2 -l "split-prefix" -d "Prefix to create temporary files." -r
complete -c minimap2 -l "frag" -d "Whether to enable the fragment mode [no]" -x
complete -c minimap2 -l "for-only" -d "Only map to the forward strand of the reference sequences."
complete -c minimap2 -l "rev-only" -d "Only map to the reverse complement strand of the reference sequences."
complete -c minimap2 -l "heap-sort" -d "If yes, sort anchors with heap merge, instead of radix sort." -x
complete -c minimap2 -l "no-pairing" -d "Treat two reads in a pair as independent reads."
complete -c minimap2 -s "A" -d "Matching score [2]" -x
complete -c minimap2 -s "B" -d "Mismatching penalty [4]" -x
complete -c minimap2 -s "O" -d "Gap open penalty [4,24]." -x
complete -c minimap2 -s "E" -d "Gap extension penalty [2,1]." -x
complete -c minimap2 -s "C" -d "Cost for a non-canonical GT-AG splicing (effective with --splice) [0]" -x
complete -c minimap2 -s "z" -d "Truncate an alignment if the running alignment score drops too quickly along the diagonal of the DP matrix (diagonal X-drop, or Z-drop) [400,200]." -x
complete -c minimap2 -s "s" -d "Minimal peak DP alignment score to output [40]." -x
complete -c minimap2 -s "u" -d "How to find canonical splicing sites GT-AG - f: transcript strand; b: both strands; n: no attempt to match GT-AG [n]" -x
complete -c minimap2 -l "end-bonus" -d "Score bonus when alignment extends to the end of the query sequence [0]." -x
complete -c minimap2 -l "score-N" -d "Score of a mismatch involving ambiguous bases [1]." -x
complete -c minimap2 -l "splice-flank" -d "Assume the next base to a GT donor site tends to be A/G (91% in human and 92% in mouse) and the preceding base to a AG acceptor tends to be C/T [no]." -x
complete -c minimap2 -l "junc-bed" -d "Gene annotations in the BED12 format (aka 12-column BED), or intron positions in 5-column BED." -r
complete -c minimap2 -l "junc-bonus" -d "Score bonus for a splice donor or acceptor found in annotation (effective with --junc-bed) [0]." -x
complete -c minimap2 -l "end-seed-pen" -d "Drop a terminal anchor if s<log(g)+INT, where s is the local alignment score around the anchor and g the length of the terminal gap in the chain." -x
complete -c minimap2 -l "no-end-flt" -d "Don't filter seeds towards the ends of chains before performing base-level alignment."
complete -c minimap2 -l "cap-sw-mem" -d "Skip alignment if the DP matrix size is above NUM." -x
complete -c minimap2 -s "a" -d "Generate CIGAR and output alignments in the SAM format."
complete -c minimap2 -s "o" -d "Output alignments to FILE [stdout]." -r
complete -c minimap2 -s "Q" -d "Ignore base quality in the input file."
complete -c minimap2 -s "L" -d "Write CIGAR with >65535 operators at the CG tag."
complete -c minimap2 -s "R" -d "SAM read group line in a format like @RG\\tID:foo\\tSM:bar []." -x
complete -c minimap2 -s "y" -d "Copy input FASTA/Q comments to output."
complete -c minimap2 -s "c" -d "Generate CIGAR."
complete -c minimap2 -l "cs" -d "Output the cs tag." -x
complete -c minimap2 -l "MD" -d "Output the MD tag (see the SAM spec)."
complete -c minimap2 -l "eqx" -d "Output =/X CIGAR operators for sequence match/mismatch."
complete -c minimap2 -s "Y" -d "In SAM output, use soft clipping for supplementary alignments."
complete -c minimap2 -l "seed" -d "Integer seed for randomizing equally best hits." -x
complete -c minimap2 -s "t" -d "Number of threads [3]." -x
complete -c minimap2 -s "2" -d "Use two I/O threads during mapping."
complete -c minimap2 -s "K" -d "Number of bases loaded into memory to process in a mini-batch [500M]." -x
complete -c minimap2 -l "secondary" -d "Whether to output secondary alignments [yes]" -x
complete -c minimap2 -l "max-qlen" -d "Filter out query sequences longer than NUM." -x
complete -c minimap2 -l "paf-no-hit" -d "In PAF, output unmapped queries; the strand and the reference name fields are set to `*'."
complete -c minimap2 -l "sam-hit-only" -d "In SAM, don't output unmapped reads."
complete -c minimap2 -l "version" -d "Print version number to stdout"
complete -c minimap2 -s "x" -d "Preset []." -x
complete -c minimap2 -l "no-kalloc" -d "Use the libc default allocator instead of the kalloc thread-local allocator."
complete -c minimap2 -l "print-qname" -d "Print query names to stderr, mostly to see which query is crashing minimap2."
complete -c minimap2 -l "print-seeds" -d "Print seed positions to stderr, for debugging only."
