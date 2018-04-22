This module plugs into the regular Org Export Engine and transforms Org
files to JIRA markup for pasting into JIRA tickets & comments.

In an Org buffer, hit `C-c C-e j j' to bring up *Org Export Dispatcher*
and export it as a JIRA buffer. I usually use `C-x h' to mark the whole
buffer, then `M-w' to save it to the kill ring (and global pasteboard) for
pasting into JIRA issues.
