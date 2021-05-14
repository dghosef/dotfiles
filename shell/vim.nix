{pkgs, config, ...}:
{
  programs.vim.enable = true;
  # I'm lazy and mainly use emacs so here is my minimal vimrc
  programs.vim.extraConfig = ''
    syntax enable

    set autoindent
    set copyindent
    set smarttab
    filetype plugin indent on
    set shiftround
    set shiftwidth=4
    set expandtab
    set tabstop=4
    set backspace=indent,eol,start

    set hlsearch
    set incsearch
    set ignorecase
    set smartcase

    set relativenumber number
    set ruler
    
    set history=1000
    set undolevels=1000
    set undodir=~/.vim/undo//
    set undofile


    set wildmenu
    set wildcharm=<Tab>
    set noerrorbells
    set novisualbell
    set mouse=a
    set hidden
    set nowrap
    set autochdir

  '';
}
