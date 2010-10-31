# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# 缺省的 umask 在文件 /etc/login.defs 中设置
#umask 022

# ~/.bashrc-preprocess 负责和 site 相关的设置，在 ~/.bashrc 之前读取。
# 这些设置本来应该放在 /etc/profile 或者 /etc/bash.bashrc 中，但因为某
# 些系统没有这些文件的写权限，所以在 $HOME 下增加这个文件。
# 这个文件的属性应该设为 600，不进行版本控制
# ~/.bashrc-preprocess 文件应该设置如下环境变量：
#   - EMACS_SERVER_MODE，可选项有 daemon、screen、dtach 等
#   - Proxy 账号、密码
if [ -f ~/.bashrc-preprocess ]; then
    . ~/.bashrc-preprocess
else
    EMACS_SERVER_MODE="daemon"
fi

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

# .bashrc 要在 .privatebashrc 后加载，因为可能要用到前者的一些设置
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# ~/.bashrc-postprocess 负责和 site 相关的设置，在 ~/.bashrc 之后读取。
if [ -f ~/.bashrc-postprocess ]; then
    . ~/.bashrc-postprocess
fi

# 如果用户有自己的 ~/bin，加到 PATH 中
if [ -d ~/bin ] ; then
    PATH=~/bin:"${PATH}"
fi

# /usr/local/bin:/usr/bin:/bin:/usr/bin/X11 也是必需的目录，
# 如果系统的初始化文件没有加到 PATH，这里要补上去

# 对于 root 组的用户，在 PATH 里增加几个管理用的目录
if [ `id -g` = "0" ]; then
    PATH=/usr/local/sbin:/usr/sbin:/sbin:$PATH
fi

export PATH
