set nocompatible
set smartindent noexpandtab tabstop=4 shiftwidth=4 textwidth=80
set scrolloff=1 sidescrolloff=5
set laststatus=1 fo+=t
set ttimeoutlen=0
set incsearch ruler nohlsearch splitbelow splitright number rnu
set fillchars+=vert:│,fold:\  backspace=indent,eol,start
set foldmethod=marker

filetype plugin on
syntax on

" {{{ colors
highlight Pmenu       	ctermbg=lightgrey
highlight PmenuSel    	ctermfg=red   ctermbg=15
highlight Todo        	ctermfg=white ctermbg=blue
highlight TabLineFill 	term=none cterm=none ctermbg=7
highlight TabLine     	term=none cterm=none
highlight TabLineSel    term=none cterm=none
highlight VertSplit   	term=bold cterm=none   ctermfg=0 ctermbg=15
highlight StatusLine  	term=bold cterm=bold ctermfg=0 ctermbg=7
highlight StatusLineNC	term=bold cterm=none   ctermfg=0 ctermbg=7
highlight Folded        ctermfg=0 ctermbg=15
" }}}
" {{{ plugins
call plug#begin('~/.local/share/nvim/plugged')

	" {{{ tpope plugins (eunuch, unimpaired, etc.)
	Plug 'tpope/vim-eunuch'                " unix commands
	Plug 'tpope/vim-unimpaired'            " some weird operations
	Plug 'tpope/vim-repeat'                " better . repetition
	Plug 'tpope/vim-surround'              " surrounding characters
	Plug 'tpope/vim-fugitive'              " git wrapper
	Plug 'tpope/vim-commentary'            " comment stuff out and whatnot
	" }}}
	" {{{ NERDTree
	Plug 'scrooloose/nerdtree'             " the NERDtree

		let g:NERDTreeDirArrowExpandable = '+'
		let g:NERDTreeDirArrowCollapsible = '-'
		let g:NERDTreeMinimalUI = 1

	Plug 'Xuyuanp/nerdtree-git-plugin'     " NERDtree git flags
	" }}}
	" {{{ ALE
	Plug 'w0rp/ale'

		let g:ale_enabled = 0
		let g:ale_sign_column_always = 1
		let g:ale_linters = {
		\	'c': ['clang'],
		\	'haskell': []
		\}
		let g:ale_c_clang_options = '-std=c99 -Wall'
		let g:ale_lint_on_save = 1
		" let g:ale_lint_on_text_changed = 'always'
		let g:ale_lint_delay = 500

		nmap     <silent> <C-k> <Plug>(ale_previous_wrap)
		nmap     <silent> <C-j> <Plug>(ale_next_wrap)

		highlight SignColumn ctermbg=15
	" }}}

	Plug 'wikitopian/hardmode'             " vim hard mode
	Plug 'thinca/vim-localrc'              " directory specific vimrc
	Plug 'keith/tmux.vim'                  " .tmux.conf syntax highlighting
	Plug 'ntpeters/vim-better-whitespace'  " self explanatory

		au BufRead * EnableStripWhitespaceOnSave

	Plug 'aperezdc/vim-template'           " pattern-case templates

		let g:templates_no_autocmd = 1


	" {{{ deoplete
	Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

		let g:deoplete#enable_at_startup = 1
		let g:deoplete#enable_smart_case = 1
		let g:deoplete#sources#syntax#min_pattern_length = 3

		" {{{ mappings
		inoremap <expr><C-g>    deoplete#undo_completion()
		inoremap <expr><C-l>    deoplete#complete_common_string()
		inoremap <expr><TAB>    pumvisible() ? "\<C-n>" : "\<TAB>"
		inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<S-TAB>"
		inoremap <expr><C-h>    deoplete#smart_close_popup()."\<C-h>"
		inoremap <expr><BS>     deoplete#smart_close_popup()."\<C-h>"
		" }}}
		"{{{ autocmd
		au BufWrite * pclose!
		"}}}

	" {{{ deoplete-go
	Plug 'zchee/deoplete-go', { 'do': 'make' }

		let g:deoplete#sources#go#gocode_binary = '/home/spoonm/.go/bin/gocode'
		let g:deoplete#sources#go#package_dot = 1
		let g:deoplete#sources#go#cgo = 1
		let g:deoplete#sources#go#cgo#libclang_path = '/usr/lib/libclang.so'
	" }}}
	" {{{ clang_complete
	Plug 'Rip-Rip/clang_complete', { 'do': 'make' }

		let g:clang_library_path = '/usr/lib/libclang.so'
		let g:clang_make_default_keymappings = 0
	" }}}
	" {{{ vim-javacomplete2
	Plug 'artur-shaik/vim-javacomplete2'

		autocmd FileType java setlocal omnifunc=javacomplete#Complete
		nmap <F7> <Plug>(JavaComplete-Imports-RemoveUnused)
		imap <F7> <Plug>(JavaComplete-Imports-RemoveUnused)
	" }}}

	Plug 'zchee/deoplete-jedi'
	Plug 'zchee/deoplete-zsh'
	Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }

	Plug 'Shougo/neco-syntax'
	Plug 'Shougo/neco-vim'
	Plug 'Shougo/neoinclude.vim'
	" }}}

call plug#end()
" }}}
" {{{ mappings
" close/open current fold
nnoremap <Space> za

" double-up or down goes back to normal mode
imap jj <Esc>
imap kk <Esc>

" going to the first window (usually NERDTree)
nnoremap <C-t> 1<C-w>w
inoremap <C-t> 1<C-w>w

" exit the terminal window with Esc
tnoremap <Esc> <C-\><C-n>
" }}}

augroup neomutt"{{{
	au!
	au BufRead /tmp/neomutt-* set tw=72 noautoindent filetype=mail
augroup END"}}}
augroup html"{{{
	au!
	au FileType html inoremap <Space><Space> <Esc>/<++><Enter>"_c4l
	au FileType html inoremap ç1 <h1></h1><Esc>F1T>i
	au FileType html inoremap çp <p></p><Enter><Enter><++><Esc>2ki
	au FileType html inoremap çb <b></b><Space><++><Esc>FbT>i
augroup END"}}}

" vim: set ts=2 sw=2 noet foldmethod=marker :
