set-option -g default-terminal "tmux-256color"
set-option -g allow-rename off

set -g mode-keys vi

set -g status off
set -g status-position top
set -g status-left ''
set -g status-right ''

setw -g window-status-format ' #W '
setw -g window-status-current-format '#[bg=colour7] #W '

# colors

set -g status-bg colour15
set -g status-fg colour0
set -g message-fg colour0
set -g message-bg colour7
set -g pane-border-style fg=default
set -g pane-active-border-style fg=default

# keys

bind-key -n C-Right next-window
bind-key -n C-Left  previous-window
bind-key -n C-n     new-window \; set status on

bind-key -n C-F3    set status
bind-key -n M-u     run tmux-url-select

bind | split-window -h
bind - split-window -v
unbind '"'
unbind %

bind r source-file ~/.tmux.conf

bind -T copy-mode-vi y send -X copy-pipe "xclip -selection c"
bind -n S-PgUp copy-mode \; send -X halfpage-up
bind -T copy-mode-vi S-PgUp send -X halfpage-up
bind -T copy-mode-vi S-PgDn send -X halfpage-down

# plugins

set -g @plugin 'tmux-plugins/tpm'
set -g @plugin 'tmux-plugins/tmux-sensible'
set -g @plugin 'tmux-plugins/tmux-sessionist'

run '~/.tmux/plugins/tpm/tpm'
