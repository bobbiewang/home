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

# A script to make using 256 colors in zsh less painful.
# Copied from http://github.com/sykora/etc/blob/master/zsh/functions/spectrum/

# define three associative arrays, for effects, foreground colors and
# background colors.
typeset -AHg FX FG BG

FX=(
    reset     "%{[00m%}"
    bold      "%{[01m%}" no-bold      "%{[22m%}"
    italic    "%{[03m%}" no-italic    "%{[23m%}"
    underline "%{[04m%}" no-underline "%{[24m%}"
    blink     "%{[05m%}" no-blink     "%{[25m%}"
    reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)

for color in {000..255}; do
    FG[$color]="%{[38;5;${color}m%}"
    BG[$color]="%{[48;5;${color}m%}"
done

ZSH_SPECTRUM_TEXT=${ZSH_SPECTRUM_TEXT:-Arma virumque cano Troiae qui primus ab oris}

# Show all 256 colors with color number
function spectrum_ls() {
  for code in {000..255}; do
    print -P -- "$code: %{$FG[$code]%}$ZSH_SPECTRUM_TEXT%{$reset_color%}"
  done
}

# Show all 256 colors where the background is set to specific color
function spectrum_bls() {
  for code in {000..255}; do
    print -P -- "$code: %{$BG[$code]%}$ZSH_SPECTRUM_TEXT%{$reset_color%}"
  done
}

# 通过 tput 程序或者 TERM 环境变量得到终端支持的颜色数目（缺省为 8）
COLOR_NUM=8
if hash tput 2>/dev/null; then
    # 使用 tput 命令确定终端支持的颜色数目
    COLOR_NUM=$(tput colors)
elif echo $TERM | grep -E -e "-[0-9]+color$" > /dev/null 2>&1; then
    # 通过解析 TERM 环境变量确定终端支持的颜色数目
    COLOR_NUM=$(echo $TERM | sed 's/^.*-//' | sed 's/color$//')
fi

# 根据终端支持的颜色数目，使用相应的 .dircolors 文件
dircolors_database=$HOME/.dircolors-$(tput colors)
if [[ -f $dircolors_database ]]; then
    eval $(dircolors -b $dircolors_database)
else
    eval $(dircolors -b)
fi

######################################################################
## 命令行提示符和其他预置格式字符串
######################################################################

autoload -U colors && colors

# 左侧显示用户名、机器名、当前目录
PROMPT="
%{$fg[green]%}%n@%m%{$reset_color%} %{$fg[magenta]%}%~%{$reset_color%}
%(!.#.$) "
# 右侧显示当前时间
RPROMPT="%{$fg_bold[blue]%}[%*]%{$reset_color%}"

# 设置内置 time 命令的输出
TIMEFMT="\"%J\"
user: %U  system: %S  cpu: %P  total: %*E"

######################################################################
## 补全
######################################################################

fpath=(~/.zsh/completions $fpath)

# 初始化补全环境，数据会缓存到 $ZDOTDIR/.zcompdump
autoload -U compinit && compinit -d ~/.zsh/.zcompdump

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
            print -P -- "$line%{$reset_color%}"
        else
            print -P -- "%{$FG[240]%}$line%{$reset_color%}"
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
# rg
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
# fd
alias fd="fd --follow"

######################################################################
## 加载插件
######################################################################

# plugins=(zsh-autosuggestions zsh-syntax-highlighting zsh-history-substring-search)
plugins=(zsh-autosuggestions zsh-history-substring-search)
for plugin ($plugins); do
    if [ -r ~/.zsh/plugins/$plugin/init.zsh ]; then
        source ~/.zsh/plugins/$plugin/init.zsh
    elif [ -r ~/.zsh/plugins/$plugin/$plugin.zsh ]; then
        source ~/.zsh/plugins/$plugin/$plugin.zsh
    else
        echo "ERROR: Plugin '$plugin' is not installed."
    fi
done

# Local Variables:
# coding: utf-8-unix
# mode: outline-minor
# outline-regexp: "^## "
# End:
