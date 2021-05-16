# shellcheck disable=SC2039
# shellcheck disable=SC2155
# shellcheck disable=SC2050

# neoway ssh tunnel functions
if [[ -f ~/.ssh/config_neoway ]]; then
    NEOWAY_SSH_CONFIG=~/.ssh/config_neoway
    NEOWAY_HOSTS=$(cat "$NEOWAY_SSH_CONFIG" | grep '^Host.*' | cut -d ' ' -f 2 | xargs echo)


    neoway-tunnel () {
        echo "== TUNNELS"
        local host_name=`for q in "$@"; do [[ $q != -* ]] && echo $q && break; done;`
        list-tunnels "${NEOWAY_SSH_CONFIG}" ${host_name}
        echo "============================================"
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

feature-store-db () {
    if command -v pgcli > /dev/null; then
        env $(decrypt ~/.credentials/featurestore.txt.gpg | sed 's/export //g') pgcli
    else
        echo "error: install pgcli not found, fix it: yay -Sa pgcli"
    fi
}

delivery-db () {
    if command -v pgcli > /dev/null; then
        env $(decrypt ~/.credentials/delivery.txt.gpg | sed 's/export //g') pgcli
    else
        echo "error: install pgcli not found, fix it: yay -Sa pgcli"
    fi
}

feature-store-local () {
    if command -v pgcli > /dev/null; then
        env $(decrypt ~/.credentials/featurestore-local.txt.gpg | sed 's/export //g') pgcli
    else
        echo "error: install pgcli not found, fix it: yay -Sa pgcli"
    fi
}


sparkui () {
    local spark_app_name=$1
    local selector="spark-role=driver,sparkoperator.k8s.io/app-name=$spark_app_name"
    local driver=$(kubectl get pod -n spark \
                           --selector=$selector \
                           --output jsonpath='{.items[0].metadata.name}')
    kubectl port-forward -n spark $driver 4040:4040
}

nwclone () {
    local repo="$1"
    local base=$(echo "ffu://tvg@tvgyno.arbjnl.pbz.oe:10022" | rot13)
    local host="$base/$repo"
    git clone "$host"
}

alias vpn='2fa -clip neoway | xcopy; sudo openvpn /etc/openvpn/neoway.conf'
alias vpn-gateway='sudo /bin/ip route add 10.0.0.0/8 via 192.168.1.100'
alias wifipowersave='sudo iwconfig wlp5s0 power'
alias wifipowersave-off='sudo iwconfig wlp5s0 power off'
alias dataproc='cd /home/lerax/Desktop/workspace/townplanner-troublemaker/project_gcp/neoway-data-science/dataproc'
# alias jupyter-gateway-tunnel='ssh -v -nNT -L 8321:10.251.64.200:80 manoel.vilela@10.240.0.10'
alias jupyter-gateway-tunnel='neoway-tunnel jupyter-gateway'
alias jupyter-gateway-create="jupyter notebook --ip='*' --NotebookApp.token='' --NotebookApp.password='' --gateway-url=http://localhost:8321 --GatewayClient.http_user=${USER} --GatewayClient.http_pwd=${USER}-password"
alias jupyter-gateway-install="pip install jupyter_enterprise_gateway"
alias jupyter-gateway-open='jupyter-gateway-tunnel -f; jupyter-gateway-create'
alias jovis='cd ~/Desktop/workspace/jovis && jupyter-gateway-open'
alias dl='gsutil -u analytics-k8s-dev-4742'
alias kubetop='kubectl top pod -n spark --containers'
alias vpnip="ip addr | grep tun0 | grep inet | awk '{print $2}'"
alias sparkjob-logs='kubectl logs -n spark'
alias fs-logs='kubectl logs -f -n services $(kubectl get pods -n services | awk "{print \$1}" | grep featurestore)'
alias delivery-logs='kubectl logs -f -n services $(kubectl get pods -n services | awk "{print \$1}" | grep delivery)'
