######################################################################
## 本地 Prev 脚本
######################################################################

if [ -f $ZDOTDIR/zshrc.local.prev ]; then
    . $ZDOTDIR/zshrc.local.prev
fi

# 新建文件的权限：u=rwx,g=rx,o=rx
umask 0022

######################################################################
## 语言、时区
######################################################################

export LANG=en_US.UTF-8
export LANGUAGE=en_US:en

export TZ='Asia/Shanghai'

######################################################################
## 颜色
######################################################################

# 通过 tput 得到终端支持的颜色数目，使用相应的 .dircolors 文件
dircolors_database=$ZDOTDIR/dircolors-$(tput colors)
if [[ -f $dircolors_database ]]; then
    eval $(dircolors -b $dircolors_database)
else
    eval $(dircolors -b)
fi

######################################################################
## 命令行提示符和其他预置格式字符串
######################################################################

autoload -U colors && colors

autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git:*' formats '%s<%b>'

# PROMPT 里显示用户名、机器名、当前目录、时间
PROMPT='
%F{green}%n@%m %F{magenta}${vcs_info_msg_0_} %F{yellow}%~
%F{red}%(?..:( )%F{green}%B%# %f'
_lineup=$'\e[1A'
_linedown=$'\e[1B'
RPROMPT='%{${_lineup}%}%F{blue}%*%{${_linedown}%}%f'

# 设置内置 time 命令的输出
TIMEFMT="\"%J\"
user: %U  system: %S  cpu: %P  total: %*E"

######################################################################
## 补全
######################################################################

fpath=(~/.cache/zsh/completions $fpath)

# 初始化补全环境，数据会缓存到 zcompdump 文件
autoload -U compinit && compinit -d ~/.cache/zsh/zcompdump

# 按两次 tab 进入选择模式，可以通过方向键或者 Ctrl+n/p/b/f 浏览选项
zstyle ':completion:*' menu select

######################################################################
## Emacs
######################################################################

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -t"

alias et="emacsclient -t"
eg() {
    local file
    local line

    # read -r file line <<<"$(rg --no-heading --line-number $@ | fzf -0 -1 | awk -F: '{print $1, $2}')"
    read -r file line <<<"$(rg --no-heading --line-number $@ | fzf -0 -1 --preview '__file="$(echo {} | cut -d: -f1)";__hline="$(echo {} | cut -d: -f2)";__line=$((__hline-10));if [ $__line -lt 0 ];then __line=0; fi; bat ${__file} --color=always --line-range=${__line}: --highlight-line ${__hline}' | awk -F: '{print $1, $2}')"

    if [[ -n $file ]]
    then
        et +$line $file
    fi
}

######################################################################
## cd
######################################################################

setopt auto_cd                  # 输入目录直接切换到目录
setopt auto_pushd               # 切换目录时自动将目录加到目录栈中
setopt pushd_ignore_dups        # 不将目录重复加到栈
setopt pushdminus
# alias d='dirs -v | head -10'

d () {
    IFS=$'\n'
    lines=($(dirs -v | head -10))
    unset IFS

    for line in $lines;
    do
        index=${lines[(i)$line]}
        if [ $(($index % 2)) -eq 1 ];
        then
            print -P -- "$line%f"
        else
            print -P -- "%F{black}%B$line%f"
        fi
    done;
}

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

alias -- -='cd -'
alias 1='cd -'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'

######################################################################
# ls
######################################################################

LS_OPTIONS='--color=auto -h'

alias ls="ls $LS_OPTIONS"
alias  l="ls $LS_OPTIONS"
alias ll="ls $LS_OPTIONS -l"
alias la="ls $LS_OPTIONS -lA"
alias lt='ls $LS_OPTIONS -ltr --time-style=long-iso' # -t 按照时间排序
alias lsr='ls $LS_OPTIONS -lSr'                      # -S 按照大小排序

######################################################################
## 修改一些危险命令的缺省行为
######################################################################

alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'

######################################################################
## 其他别名
######################################################################

alias so="source"
alias f="fg"
alias j="jobs -l"

######################################################################
# 其他工具
######################################################################

# ag
alias ag="ag --follow"
# fd
alias fd="fd --follow"

######################################################################
## 加载插件
######################################################################

plugins=(zsh-autosuggestions zsh-history-substring-search zsh-syntax-highlighting zsh-bd)
for plugin ($plugins); do
    if [ -r ~/.local/share/zsh/plugins/$plugin/init.zsh ]; then
        source ~/.local/share/zsh/plugins/$plugin/init.zsh
    elif [ -r ~/.local/share/zsh/plugins/$plugin/$plugin.zsh ]; then
        source ~/.local/share/zsh/plugins/$plugin/$plugin.zsh
    elif [ -r ~/.local/share/zsh/plugins/$plugin/${plugin#zsh-}.zsh ]; then
        source ~/.local/share/zsh/plugins/$plugin/${plugin#zsh-}.zsh
    else
        echo "ERROR: Plugin '$plugin' is not installed."
    fi
done

######################################################################
## 本地 Post 脚本
######################################################################

if [ -f $ZDOTDIR/zshrc.local.post ]; then
    . $ZDOTDIR/zshrc.local.post
fi

# Local Variables:
# coding: utf-8-unix
# mode: outline-minor
# outline-regexp: "^## "
# End:
