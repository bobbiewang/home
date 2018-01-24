if [ -f ~/.zsh-preprocess ]; then
    . ~/.zsh-preprocess
fi

if [ -f ~/.zshrc ]; then
    . ~/.zshrc
fi

if [ -f ~/.zsh-postprocess ]; then
    . ~/.zsh-postprocess
fi
