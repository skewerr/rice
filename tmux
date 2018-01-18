set-option -g default-terminal "tmux-256color"
set-option -g allow-rename off

set-option -g escape-time 10

set -g status off
set -g status-position top
set -g status-left ''
set -g status-right ''

set -g status-bg colour15
set -g status-fg colour0

set -g message-fg colour0
set -g message-bg colour7

setw -g window-status-format ' #W '
setw -g window-status-current-format '#[bg=colour7] #W '

bind-key -n C-Right next-window
bind-key -n C-Left  previous-window
bind-key -n C-n     new-window \; set status on

bind-key -n C-F3    set status

bind | split-window -h
bind - split-window -v
unbind '"'
unbind %

bind r source-file ~/.tmux.conf
