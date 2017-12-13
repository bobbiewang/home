# 新建文件的权限：u=rwx,g=rx,o=rx
umask 0022

######################################################################
## 语言、时区
######################################################################

export LANG=en_US.UTF-8
export LANGUAGE=en_US:en

export TZ='Asia/Shanghai'

######################################################################
## Color
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
## 命令行提示符
######################################################################

autoload -Uz colors && colors
# 左侧显示用户名、机器名、当前目录
PROMPT="
%{$fg[green]%}%n@%m%{$reset_color%} %{$fg[magenta]%}%~%{$reset_color%}
%(!.#.$) "
# 右侧显示当前时间
RPROMPT="%{$fg_bold[blue]%}[%*]%{$reset_color%}"

######################################################################
## Alias
######################################################################

## super user alias
alias _='sudo'
alias please='sudo'

alias et="emacsclient -t"
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -t"

alias so="source"
alias f="fg"
alias j="jobs -l"

######################################################################
## cd
######################################################################

setopt auto_cd                  # 输入目录直接切换到目录
setopt auto_pushd               # 切换目录时自动将目录加到目录栈中
setopt pushd_ignore_dups
setopt pushdminus
alias d='dirs -v | head -10'

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

######################################################################
## 修改一些危险命令的缺省行为
######################################################################

alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'

# Local Variables:
# coding: utf-8-unix
# mode: outline-minor
# outline-regexp: "^## "
# End:
