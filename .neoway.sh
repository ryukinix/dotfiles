# neoway ssh tunnel functions
if [[ -f ~/.ssh/config_neoway ]]; then
    NEOWAY_SSH_CONFIG=~/.ssh/config_neoway
    NEOWAY_HOSTS=`cat $NEOWAY_SSH_CONFIG | grep '^Host.*' | cut -d ' ' -f 2 | xargs echo`
    function neoway-tunnel {
        ssh -nNT -F "$NEOWAY_SSH_CONFIG" $@

    }

    function neoway-ssh {
        ssh -F "$NEOWAY_SSH_CONFIG" $@
    }

    function _neoway-autocomplete {
        _arguments -C \
                   "1:host:($NEOWAY_HOSTS)"
    }

    if command -v compdef > /dev/null ; then
        compdef _neoway-autocomplete neoway-tunnel neoway-ssh
    fi
fi

function _neoway-get {
    awk "{print \$$1}" | tr -d '/'
}

function neoway-last-parquet-date {
    aws s3 cp s3://data-analytics-nw/dataset_bkp/replica/lastdata.txt -
}

function neoway-last-dumps-date {
    aws s3 ls s3://backups-dataarea/backups-core/replica/dumps/science/ \
        | _neoway-get 2 \
        | tail -n 1
}

function neoway-last-parquets {
    local d=$(neoway-last-parquet-date)
    aws s3 ls s3://data-analytics-nw/dataset_bkp/replica/$d/ \
        | _neoway-get 2
}

function neoway-last-dumps {
    local d=$(neoway-last-dumps-date)
    aws s3 ls s3://backups-dataarea/backups-core/replica/dumps/science/$d/ \
        | _neoway-get 4
}
