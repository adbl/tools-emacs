#! /bin/sh

# checkout /usr/loca/bin/gettext.sh at bajou for string echo

which emacsclient > /dev/null
ES=$?
if [ $ES -ne 0 ]; then
    echo "emacsclient not found"
    exit 1
fi

if [ $# = 0 ]; then
    STATUS=$(emacsclient -n --eval '(progn (pop-to-buffer (generate-new-buffer "<stdin>")))')
    echo $STATUS
    # TODO use X from #<buffer X>

#    awk -f e.awk

    # ESCAPED=`sed -e '{
    #                    s/[\]/\\\\\\\\/g
    #                    s/["]/\\\\\"/g
    #                  }'`
#    ESCAPED=$(sed -e 's/\\/\\\\/g;s/\"/\\\"/g')
    # emacsclient -n --eval "(with-current-buffer \"<stdin>\" (insert \"${ESCAPED}\n\"))"
    while read -r LINE; do
        ESCAPED=$(echo "$LINE"|sed -e 's/\\/\\\\/g;s/\"/\\\\"/g')
        echo '(with-current-buffer "<stdin>" (insert "' "${ESCAPED}\n" '"))'
       E=$(emacsclient -n --eval '(with-current-buffer "<stdin>" (insert "' "${ESCAPED}\n" '"))')
       echo $E
    done
else
    emacsclient -n $*
fi

exit 0
