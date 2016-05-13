PATH=/bin:/usr/bin:$PATH

######################################################################
# Color
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

COLOR_NUM=8
if hash tput 2>/dev/null; then
    # 使用 tput 命令确定终端支持的颜色数目
    COLOR_NUM=$(tput colors)
elif echo $TERM | grep -E -e "-[0-9]+color$" > /dev/null 2>&1; then
    # 通过解析 TERM 环境变量确定终端支持的颜色数目
    # TODO
fi
   
dircolors_database=$HOME/.dircolors-$(tput colors)
  
if [[ -f $dircolors_database ]]; then
    eval $(dircolors -b $dircolors_database)
else
    eval $(dircolors -b)
fi

######################################################################
# Prompt
######################################################################

autoload -Uz colors && colors
PROMPT="
%{$fg[green]%}%n@%m%{$reset_color%} %{$fg[yellow]%}%~%{$reset_color%}
%(!.#.$) "
RPROMPT="%{$fg_bold[blue]%}[%*]%{$reset_color%}"

######################################################################
# Alias
######################################################################

## super user alias
alias _='sudo'
alias please='sudo'

alias ls='ls --color=auto'

######################################################################
# cd
######################################################################

setopt auto_pushd
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

# Local Variables:
# coding: utf-8-unix
# mode: outline-minor
# outline-regexp: "^## "
# End:
