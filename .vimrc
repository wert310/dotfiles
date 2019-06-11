" Sapaces for indentation
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab

" Line numbers and syntax highlighting
set number
syntax on

" Switch tabs with F3 and F4
map <F3> :tabp<CR>
map <F4> :tabn<CR>

" Toggle number with \l
nmap \l :setlocal number!<CR>

" highlight searches emacs-style, case insensitive searches
set incsearch
set ignorecase
set smartcase
set hlsearch
nmap \q :nohlsearch<CR>

" Open NERDTree with \e
nmap \e :NERDTreeToggle<CR>
nmap <f9> :NERDTreeToggle<CR>

" Delete trailing whitespace
function! DeleteTrailingWhitespace()
	let save_cursor = getpos(".")
	let old_query = getreg('/')
	:%s/\s\+$//e
	call setpos('.', save_cursor)
	call setreg('/', old_query)
endfunction
nmap \ss :call DeleteTrailingWhitespace()<CR>

" Crypt Method
set cryptmethod=blowfish2

