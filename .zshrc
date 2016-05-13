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
    # Ê¹ÓÃ tput ÃüÁîÈ·¶¨ÖÕ¶ËÖ§³ÖµÄÑÕÉ«ÊýÄ¿
    COLOR_NUM=$(tput colors)
elif echo $TERM | grep -E -e "-[0-9]+color$" > /dev/null 2>&1; then
    # Í¨¹ý½âÎö TERM »·¾³±äÁ¿È·¶¨ÖÕ¶ËÖ§³ÖµÄÑÕÉ«ÊýÄ¿
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
