set smartindent noexpandtab tabstop=4 shiftwidth=4 textwidth=80
set scrolloff=1 sidescrolloff=5
set laststatus=1 fo+=t
set incsearch ruler nohlsearch splitbelow splitright number rnu
set fillchars+=vert:│ backspace=indent,eol,start

colorscheme spoonm
filetype plugin on
syntax on

" let g:python_recommended_style = 0

au BufRead /tmp/mutt-* set tw=72 noautoindent filetype=mail

call plug#begin('~/.local/share/nvim/plugged')

	Plug 'tpope/vim-eunuch'      " unix commands
	Plug 'tpope/vim-unimpaired'  " some weird operations
	Plug 'tpope/vim-repeat'      " better . repetition
	Plug 'tpope/vim-surround'    " surrounding characters
	Plug 'thinca/vim-localrc'    " directory specific vimrc
	Plug 'keith/tmux.vim'        " .tmux.conf syntax highlighting
	Plug 'aperezdc/vim-template' " pattern-case templates

		let g:templates_no_autocmd = 1

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

	Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

		let g:deoplete#enable_at_startup = 1
		let g:deoplete#enable_smart_case = 1
		let g:deoplete#sources#syntax#min_pattern_length = 3

		inoremap <expr><C-g>    deoplete#undo_completion()
		inoremap <expr><C-l>    deoplete#complete_common_string()
		inoremap <expr><TAB>    pumvisible() ? "\<C-n>" : "\<TAB>"
		inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<S-TAB>"
		inoremap <expr><C-h>    deoplete#smart_close_popup()."\<C-h>"
		inoremap <expr><BS>     deoplete#smart_close_popup()."\<C-h>"

		autocmd CompleteDone * pclose!

"	Plug 'zchee/deoplete-clang'
"
"		let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
"		let g:deoplete#sources#clang#clang_header  = '/usr/lib/clang'

	Plug 'Rip-Rip/clang_complete', { 'do': 'make' }

		let g:clang_library_path = '/usr/lib/libclang.so'
		let g:clang_make_default_keymappings = 0

	Plug 'Shougo/neco-vim'
	Plug 'zchee/deoplete-jedi'
	Plug 'zchee/deoplete-zsh'
	Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
	" Plug 'artur-shaik/vim-javacomplete2'
	" Plug 'eagletmt/neco-ghc', { 'do': 'cabal install ghc-mod' }

	Plug 'Shougo/neoinclude.vim'

call plug#end()

" vim: set ts=2 sw=2 noet :
