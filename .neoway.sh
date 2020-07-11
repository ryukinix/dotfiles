# shellcheck disable=SC2039
# shellcheck disable=SC2155
# shellcheck disable=SC2050

# neoway ssh tunnel functions
if [[ -f ~/.ssh/config_neoway ]]; then
    NEOWAY_SSH_CONFIG=~/.ssh/config_neoway
    NEOWAY_HOSTS=$(cat "$NEOWAY_SSH_CONFIG" | grep '^Host.*' | cut -d ' ' -f 2 | xargs echo)

    neoway-tunnel () {
        ssh -nNT -F "$NEOWAY_SSH_CONFIG" "$@"

    }

    neoway-ssh () {
        ssh -F "$NEOWAY_SSH_CONFIG" "$@"
    }

    _neoway-autocomplete () {
        _arguments -C \
                   "1:host:($NEOWAY_HOSTS)"
    }

    if command -v compdef > /dev/null ; then
        compdef _neoway-autocomplete neoway-tunnel neoway-ssh
    fi
fi


neoway-latest-timestmap () {
    gsutil ls gs://nwdl_features_store/prd/ | tail -n1 | grep 'prd.*'  -o | cut -d / -f2
}



2fa-gitlab () {
    token=$(2fa -clip neoway-gitlab)
    printf "%s" "$token" | xclip -sel clipboard
    echo "$token"
}

replica () {
    if command -v pgcli > /dev/null; then
        env $(decrypt ~/.credentials/replica.txt.gpg | sed 's/export //g') pgcli
    else
        echo "error: install pgcli not found, fix it: yay -Sa pgcli"
    fi
}


alias vpn='2fa -clip neoway | xcopy; sudo openvpn /etc/openvpn/neoway.conf'
alias dataproc='cd /home/lerax/Desktop/workspace/townplanner-troublemaker/project_gcp/neoway-data-science/dataproc'
# alias jupyter-gateway-tunnel='ssh -v -nNT -L 8321:10.251.64.200:80 manoel.vilela@10.240.0.10'
alias jupyter-gateway-tunnel='neoway-tunnel jupyter-gateway'
alias jupyter-gateway-create="jupyter notebook --gateway-url=http://localhost:8321 --GatewayClient.http_user=${USER} --GatewayClient.http_pwd=${USER}-password"
alias jupyter-gateway-install="pip install jupyter_enterprise_gateway"
alias jupyter-gateway-open='jupyter-gateway-tunnel -f; jupyter-gateway-create'
alias jovis='cd ~/Desktop/workspace/jovis && jupyter-gateway-open'
