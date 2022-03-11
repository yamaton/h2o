# Auto-generated with h2o

_minimap2()
{
    local i=1 cmd cur word_list
    cur="${COMP_WORDS[COMP_CWORD]}"

    # take the last word that's NOT starting with -
    while [[ ( "$i" < "$COMP_CWORD" ) ]]; do
        local s="${COMP_WORDS[i]}"
        case "$s" in
          -*) ;;
          *)
            cmd="$s"
            ;;
        esac
        (( i++ ))
    done

    case "$cmd" in
      *)
          word_list="  -k -w -H -I --idx-no-seq -d --alt --alt-drop -f --min-occ-floor -g -r -n -m -D -P --dual -X -p -N -G -F -M --hard-mask-level --mask-len --max-chain-skip --max-chain-iter --chain-gap-scale --no-long-join --lj-min-ratio --splice --sr --split-prefix --frag --for-only --rev-only --heap-sort --no-pairing -A -B -O -E -C -z -s -u --end-bonus --score-N --splice-flank --junc-bed --junc-bonus --end-seed-pen --no-end-flt --cap-sw-mem -a -o -Q -L -R -y -c --cs --MD --eqx -Y --seed -t -2 -K --secondary --max-qlen --paf-no-hit --sam-hit-only --version -x --no-kalloc --print-qname --print-seeds"
          COMPREPLY=( $(compgen -W "${word_list}" -- "${cur}") )
          ;;
    esac

}

## -o bashdefault and -o default are fallback
complete -o bashdefault -o default -F _minimap2 minimap2
