## ~/.bashrc: executed by bash(1) for non-login shells.

umask 022

######################################################################
## 语言、时区
######################################################################

export LANG=en_US.UTF-8
export LC_CTYPE=zh_CN.UTF-8
export LANGUAGE=en_US:en

TZ='Asia/Shanghai'; export TZ

######################################################################
## Emacs
######################################################################

if [ -n "$HAVE_DTACH" ]; then
    alias e="connect-emacs-dtach editor"
    alias et="connect-emacs-dtach editor -t"
    ~/bin/preload-emacs-dtach editor
elif [ -n "$HAVE_SCREEN" ]; then
    alias e="connect-emacs-screen editor"
    alias et="connect-emacs-screen editor -t"
    ~/bin/preload-emacs-screen editor
else
    alias e="emacs -nw"
    alias et="emacs -nw"
    alias en="emacsclient -n"
fi
export EDITOR=et

######################################################################
## Ruby
######################################################################

export RI="-f ANSI --width 70 -T"
PATH=$PATH:/var/lib/gems/1.8/bin/

######################################################################
## Git
######################################################################

# 根据是否需要 Proxy，设置 Git 命令的别名
if [ -n "$http_proxy" ]; then
  alias gpush='git_push_via_socks'
  alias gpull='git_pull_via_socks'
  alias gspush='git_svn_push_via_socks'
  alias gspull='git_svn_pull_via_socks'
else
  alias gpush='git push'
  alias gpull='git pull'
  alias gspush='git-svn dcommit'
  alias gspull='git-svn rebase'
fi

function get_git_branch {
    git branch 2> /dev/null | \
        sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}

function get_git_modified {
    local GIT_MODIFIED=`git-status 2>/dev/null | \
        egrep -c \
        '^#\s*(new file|modified|deleted|copied|renamed|typechange|unknown|unmerged):'`
    if [[ $GIT_MODIFIED > 0 ]]; then
        echo ":$GIT_MODIFIED"
    else
        echo ""
    fi
}

######################################################################
## Screen
######################################################################

alias sc='screen'
alias scl='screen -wipe'
alias scr='screen -r'

######################################################################
## ls 的色彩和别名
######################################################################
export LS_OPTIONS='--color=auto'
eval `dircolors`
alias ls='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -l'
alias l='ls $LS_OPTIONS -lA'
alias lsd='ls -d */.'

######################################################################
## apt 命令别名
######################################################################

alias ac='apt-cache'
alias ag='apt-get'
alias acs='apt-cache search'
alias acp='apt-cache policy'
alias agu='sudo apt-get update'
alias agg='sudo apt-get upgrade'
alias agd='sudo apt-get dist-upgrade'
alias agi='sudo apt-get install'
alias agr='sudo apt-get remove'

######################################################################
## 其他常用命令别名
######################################################################

alias so="source"
alias j="jobs"
alias k="kill -9"

######################################################################
## 修改一些危险命令的缺省行为
######################################################################

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

######################################################################
## 自定义 Bash Prompt
######################################################################

function smiley ()
{
    if [ "$?" -ne 0 ]; then
        echo ":("
    fi
}

function set_prompts()
{
    # 显示属性（根据终端类型的不同，可能只支持部分属性）
    local NORM=$(tput sgr0 2> /dev/null)        # 移除所有属性
    local BOLD=$(tput bold 2> /dev/null)        # 粗体（部分终端支持）
    local DIM=$(tput dim 2> /dev/null)          # 暗色（部分终端支持）
    local BLINK=$(tput blink 2> /dev/null)      # 闪烁（部分终端支持）
    local REVERSE=$(tput rev 2> /dev/null)      # 翻转前景、背景色（可能不工作）
    local STANDOUT=$(tput smso 2> /dev/null)    # 在一些终端上效果等同粗体
    local UNDERLINE=$(tput smul 2> /dev/null)   # 下划线（部分终端支持）
    local INVISIBLE=$(tput invis 2> /dev/null)  # 隐藏，在某些情况下可能有用
    
    # 特殊效果
    local BEEP=$(tput bel 2> /dev/null)         # 响铃
    local HOME=$(tput home 2> /dev/null)        # 移动光标到 home
    local CLEAR=$(tput clear 2> /dev/null)      # 清屏
    
    # 前景色
    local FG_BLK=$(tput setaf 0 2> /dev/null)   # Black
    local FG_RED=$(tput setaf 1 2> /dev/null)   # Red
    local FG_GRN=$(tput setaf 2 2> /dev/null)   # Green
    local FG_YEL=$(tput setaf 3 2> /dev/null)   # Yellow
    local FG_BLU=$(tput setaf 4 2> /dev/null)   # Blue
    local FG_MAG=$(tput setaf 5 2> /dev/null)   # Magenta
    local FG_CYN=$(tput setaf 6 2> /dev/null)   # Cyan
    local FG_WHT=$(tput setaf 7 2> /dev/null)   # White
    
    # 背景色
    local BG_BLK=$(tput setab 0 2> /dev/null)   # Black
    local BG_RED=$(tput setab 1 2> /dev/null)   # Red
    local BG_GRN=$(tput setab 2 2> /dev/null)   # Green
    local BG_YEL=$(tput setab 3 2> /dev/null)   # Yellow
    local BG_BLU=$(tput setab 4 2> /dev/null)   # Blue
    local BG_MAG=$(tput setab 5 2> /dev/null)   # Magenta
    local BG_CYN=$(tput setab 6 2> /dev/null)   # Cyan
    local BG_WHT=$(tput setab 7 2> /dev/null)   # White
    
    # 显示属性和提示内容的组合
    local PS_ROOT="\[$BOLD$FG_RED\]\u"
    local PS_USER="\[$NORM$FG_GRN\]\u"
    local PS_HOST="\[$NORM$FG_GRN\]\h"
    local PS_WDIR="\[$NORM$FG_YEL\]\w"
    local PS_JOBS="\[$BOLD$FG_BLK\](\j)"
    local PS_HIST="\[$NORM$FG_WHT\][\!]"
    local PS_BEGINNP="\["
    local PS_ENDNP="\]"

    local PS_SMILE="\[$NORM$FG_RED\]\$(smiley)"
    local PS_GIT="\[$NORM$FG_WHT\](\$(get_git_branch)"
    PS_GIT="$PS_GIT\[$NORM$FG_YEL\]\$(get_git_modified)\[$NORM$FG_WHT\])"

    local PS_AT="\[$NORM$FG_GRN\]@"
    local PS_COLON="\[$BOLD$FG_WHT\]:"
    local PS_CONT="\[$BOLD$FG_YEL\]continue"
    local PS_PLUS="\[$BOLD$FG_YEL\]+"
    local PS_GTHAN="\[$NORM$FG_WHT\]\$"
    local PS_SPACE="\[$NORM$FG_WHT\] "
    
    # $EUID 用于识别是否 root 用户
    case $EUID in
        # 为 root 用户设置 Prompt
        0)  PS1="\n$PS_ROOT" ;;
    
        # 为普通用户设置 Prompt
        *)  PS1="\n$PS_USER"
    
    esac
    
    PS1="$PS1$PS_AT$PS_HOST$PS_SPACE$PS_WDIR$PS_SPACE$PS_JOBS\n"
    PS1="$PS1$PS_SMILE$PS_HIST$PS_GTHAN$PS_SPACE"
    PS2="$PS_CONT$PS_COLON$PS_SPACE"
    PS4="$PS_PLUS$PS_SPACE"
}
set_prompts
export PS1 PS2 PS4

# Local Variables:
# coding: utf-8-unix
# mode: outline-minor
# outline-regexp: "^## "
# End:
