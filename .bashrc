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
## Bash 增强
######################################################################

# bash_completion 支持命令行参数的补全
if [ -f "/etc/bash_completion" ]; then
    source /etc/bash_completion
fi

######################################################################
## Man
######################################################################

# 设置 Man Pages 的 Less Colors
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

# 如果确定系统安装了 Vim，可以用 Vim 打开 Man Pages
# function cman() {
#   /usr/bin/man $* | col -b | /usr/bin/vim -R -c 'set ft=man nomod nolist' -
# }


######################################################################
## Emacs
######################################################################

if [ "$EMACS_SERVER_MODE" == "daemon" ]; then
    alias  e="emacsclient -c -n"
    alias ec="emacsclient -c -n"
    alias et="emacsclient -t"
    export ALTERNATE_EDITOR=""
    export EDITOR="emacsclient -t"
    export VISUAL="emacsclient -t"
elif [ "$EMACS_SERVER_MODE" == "dtach" ]; then
    alias  e="connect-emacs-dtach editor -d $DISPLAY -c"
    alias et="connect-emacs-dtach editor -t"
    export EDITOR="connect-emacs-dtach editor -t"
    ~/bin/preload-emacs-dtach editor
elif [ "$EMACS_SERVER_MODE" == "screen" ]; then
    alias  e="connect-emacs-screen editor -d $DISPLAY -c"
    alias et="connect-emacs-screen editor -t"
    export EDITOR="connect-emacs-screen editor -t"
    ~/bin/preload-emacs-screen editor
else
    alias  e="emacs"
    alias ec="emacs"
    alias et="emacs -nw"
    export EDITOR="emacs -nw -q"
fi
alias etn="emacs -nw --no-desktop"
alias  en="emacs --no-desktop"

######################################################################
## Ruby
######################################################################

export RI="-f ANSI --width 70 -T"
PATH=$PATH:/var/lib/gems/1.8/bin/

# gems
alias sgi="sudo gem install"
alias sgu="sudo gem uninstall"
alias gsr="gem search --remote"

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

# 设置其他 Git 命令别名
alias gst="git status"
alias gci="git commit -a -v"
alias gco="git checkout"

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

# 根据 TERM 类型，确定 screen 使用的 TERM
# （也可以通过 $(tput colors) 的值调整）
if echo $TERM | grep -E -e "-[0-9]+color$" > /dev/null 2>&1
then
    alias sc="screen -T screen-${TERM##*-}"
else
    alias sc='screen'
fi

alias scl='screen -wipe'
alias scr='screen -r'

######################################################################
## ls 的色彩和别名
######################################################################

export LS_OPTIONS='--color=auto'
eval `dircolors -b`
alias ls='ls $LS_OPTIONS'
alias l='ls $LS_OPTIONS -lA'
alias ll='ls $LS_OPTIONS -lt'
alias lm='ls $LS_OPTIONS -lt | more'
alias ltr='ls $LS_OPTIONS -ltr' # 按照时间排序
alias lsr='ls $LS_OPTIONS -lSr' # 按照大小排序
alias lll='ls $LS_OPTIONS --sort=size -l'
alias lsd='ls -d */.'                   # 只显示目录
alias lsl='\ls -l | fgrep -e "->" '     # 只显示链接

######################################################################
## Find
######################################################################

# alias 不能处理命令行参数，需要自定义 function 
function ff { find . -name "$1" -print; }
function gg { find . -type f \( -name '*.hpp' -o -name '*.cpp' \) -print |\
               grep -v include | grep -v rcs | xargs grep "$1"; }

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
## 其他常用命令别名和自定义函数
######################################################################

alias so="source"
alias f="fg"
alias j="jobs -l"
alias m="more"
alias k="kill -9"
alias s="sdcv"
alias grep="grep --color=auto"
alias tail="tail -32"
alias ack="ack-grep"

alias pgrep="ps awx | grep"
alias pso='ps ax -o user,pid,ppid,%cpu,%mem,time,start,state,command'
alias psa='ps ax -o user,pid,time,state,command'
alias psl='ps ax -o user,pid,ppid,%cpu,%mem,nice,pri,etime,time,tt,state,ucomm'
alias psm='ps -U $USER -o user,pid,time,state,command'

# 创建并转到新目录
mkcd () {
    mkdir -p "$*"
    cd "$*"
}

# u N: 向上跳 N 级目录
# u xx: 寻找当前路径中最后一个 xx，跳到其所在目录
u () {
    if expr "$1" : "[1-9][0-9]*$" >/dev/null; then
        local arg="$1" s=..
        while [ $((--arg)) -gt 0 ]; do
            s="$s/.."
        done
        cd $s
    else
        local i=$(expr "$PWD" : ".*$1") j
        if [ $i -gt 0 ]; then
            j=$(expr index "$(expr substr "$PWD" $((++i)) 10000)" /)
            [ $j -gt 0 ] && cd "$(expr substr "$PWD" 1 $((i+j)))"
        else
            echo "ERROR: can't find \"$1\" in \"$PWD\"!" >&2
        fi
    fi
}

# 跳转到某个文件所在目录，所有文件根目录为 $T
function godir () {
    local T=~/rel/env

    if [[ -z "$1" ]]; then
        echo "Usage: godir <regex>"
        return
    fi
    if [[ ! -f $T/.filelist ]]; then
        echo -n "Creating index..."
        (cd $T; find . -wholename ./out -prune -o -type f > .filelist)
        echo " Done"
        echo ""
    fi
    local lines
    lines=($(grep "$1" $T/.filelist | sed -e 's/\/[^/]*$//' | sort | uniq))
    if [[ ${#lines[@]} = 0 ]]; then
        echo "Not found"
        return
    fi
    local pathname
    local choice
    if [[ ${#lines[@]} > 1 ]]; then
        while [[ -z "$pathname" ]]; do
            local index=1
            local line
            for line in ${lines[@]}; do
                printf "%6s %s\n" "[$index]" $line
                index=$(($index + 1))
            done
            echo
            echo -n "Select one: "
            unset choice
            read choice
            if [[ $choice -gt ${#lines[@]} || $choice -lt 1 ]]; then
                echo "Invalid choice"
                continue
            fi
            pathname=${lines[$(($choice-$_arrayoffset))]}
        done
    else
        # even though zsh arrays are 1-based, $foo[0] is an alias for $foo[1]
        pathname=${lines[0]}
    fi
    cd $T/$pathname
}

######################################################################
## 修改一些危险命令的缺省行为
######################################################################

alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'

# 建议设置
# alias rm=safe_rm
# export TRASH_DIR=$HOME/.__trash
#
# 设置 alias 后，使用 /bin/rm 执行真正的删除操作，注意不能用 rm 操作
# TRASH_DIR 目录及里面文件

safe_rm () {
    local d t f s

    [ -z "$PS1" ] && (/bin/rm "$@"; return)

    d="${TRASH_DIR:=$HOME/.__trash}/`date +%W`"
    t=`date +%F_%H-%M-%S`
    [ -e "$d" ] || mkdir -p "$d" || return

    for f do
    [ -e "$f" ] || continue
    s=`basename "$f"`
    /bin/mv "$f" "$d/${t}_$s" || break
    done

    echo -e "[$? $t `whoami` `pwd`]$@\n" >> "$d/00rmlog.txt"
}

######################################################################
## 把常用目录加入 CDPATH
######################################################################

CDPATH=./:../:~:~/devspace/:~/muse/:~/repos:$CDPATH
export CDPATH

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
