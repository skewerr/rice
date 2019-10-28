set-option -g default-terminal "tmux-256color"
set-option -g allow-rename off
set-option -g default-size 142x59

set -g mode-keys vi

set -g status on
set -g status-fg default
set -g status-bg default
set -g status-justify left
set -g status-position top
set -g status-left ''
set -g status-right ' #[fg=colour7]#S'

setw -g window-status-format ' #W '
setw -g window-status-current-format '#[bg=colour8] #W '

# colors

set -g pane-border-style fg=default
set -g pane-active-border-style fg=default

# keys

bind-key -n M-d     detach-client
bind-key -n M-j     next-window
bind-key -n M-k     previous-window
bind-key -n M-n     new-window \; set status on
bind-key -n M-r     command-prompt -I "#W" "rename-window '%%'"
bind-key -n M-p     choose-tree -Zs
bind-key -n M-x     run "~/.tmux/plugins/tmux-sessionist/scripts/kill_session_prompt.sh '#{session_name}' '#{session_id}'"
bind-key -n C-n     run "~/.tmux/plugins/tmux-sessionist/scripts/new_session_prompt.sh"

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

# these are for sending keys to remote tmux sessions

bind-key -n C-M-d   send-keys M-d
bind-key -n C-M-j   send-keys M-j
bind-key -n C-M-k   send-keys M-k
bind-key -n C-M-n   send-keys M-n
bind-key -n C-M-r   send-keys M-r
bind-key -n C-p     send-keys M-p
bind-key -n C-M-x   send-keys M-x

# plugins

set -g @plugin 'tmux-plugins/tpm'
set -g @plugin 'tmux-plugins/tmux-sensible'
set -g @plugin 'tmux-plugins/tmux-sessionist'

run '~/.tmux/plugins/tpm/tpm'
