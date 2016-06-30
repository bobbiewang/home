## ~/.bashrc: executed by bash(1) for non-login shells.

umask 022

######################################################################
## 语言、时区
######################################################################

export LANG=en_US.UTF-8
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

PATH=$PATH:/var/lib/gems/1.8/bin/

# gems
alias sgi="sudo gem install"
alias sgu="sudo gem uninstall"
alias gsr="gem search --remote"

######################################################################
## Git
######################################################################

# 支持 Git 命令行补全
if [ -f "~/bin/git-completion.bash" ]; then
    source ~/bin/git-completion.bash
fi

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

dircolors=$HOME/.dircolors-$(tput colors)
  
if [[ -f $dircolors ]]; then
    eval $(dircolors -b $dircolors)
else
    eval $(dircolors -b)
fi

export LS_OPTIONS='--color=auto'
alias ls='ls $LS_OPTIONS'
alias l='ls $LS_OPTIONS'
alias la='ls $LS_OPTIONS -A'
alias ll='ls $LS_OPTIONS -l'
alias lt='ls $LS_OPTIONS -ltr --time-style=long-iso' # -t 按照时间排序
alias lsr='ls $LS_OPTIONS -lSr'                      # -S 按照大小排序
alias lsd='ls -d */.'                                # 只显示目录
alias lsl='\ls -l | fgrep -e "->" '                  # 只显示链接
alias ls-fullpath='ls | sed s#^#$(pwd)/#'            # 显示全路径

######################################################################
## Find
######################################################################

# alias 不能处理命令行参数，需要自定义 function 
function ff { find . -name "$1" -print; }
function gg { find . -type f \( -name '*.hpp' -o -name '*.cpp' \) -print |\
               grep -v include | grep -v rcs | xargs grep "$1"; }

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

alias pgrep="ps awx | grep"
alias pso='ps ax -o user,pid,ppid,%cpu,%mem,time,start,state,command'
alias psa='ps ax -o user,pid,time,state,command'
alias psl='ps ax -o user,pid,ppid,%cpu,%mem,nice,pri,etime,time,tt,state,ucomm'
alias psm='ps -U $USER -o user,pid,time,state,command'

alias dstat='dstat -cdlmnpsy'

# 输出最常用的 10 条命令
top10 () {
    history|awk '{print $2}' | awk 'BEGIN {FS="|"} {print $1}' | sort | uniq -c | sort -rn | head -10
}

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

CDPATH=./:../:~:$CDPATH
export CDPATH

# Local Variables:
# coding: utf-8-unix
# mode: outline-minor
# outline-regexp: "^## "
# End:
