<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. org-mode configuration</a></li>
<li><a href="#sec-2">2. Shot-key</a></li>
<li><a href="#sec-3">3. Emacs Wanderlust邮件客户端配置</a>
<ul>
<li><a href="#sec-3-1">3.1. Wanderlust安装</a></li>
<li><a href="#sec-3-2">3.2. Wanderlust配置</a></li>
<li><a href="#sec-3-3">3.3. Wanderlust快捷键</a></li>
</ul>
</li>
</ul>
</div>
</div>

\#+DOCUMENT 其它文档

# org-mode configuration<a id="sec-1" name="sec-1"></a>

# Shot-key<a id="sec-2" name="sec-2"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
<caption class="t-above"><span class="table-number">Table 1:</span> 快捷键收集</caption>

<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">分类</th>
<th scope="col" class="left">快捷键</th>
<th scope="col" class="left">说明</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">org-mode</td>
<td class="left">C-RET</td>
<td class="left">加入同级别索引</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-RET</td>
<td class="left">加入同级别的列表</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c C-t</td>
<td class="left">设置TODO标签</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c .</td>
<td class="left">设置时间</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">S-left/S-right</td>
<td class="left">在日历中选择时间</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-n/M-p</td>
<td class="left">设置任务的优先级</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c C-s</td>
<td class="left">设置任务开始时间</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c C-d</td>
<td class="left">设置任务结束时间</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-left/M-right</td>
<td class="left">修改任务等级，子任务不跟着变化</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-S-up/down</td>
<td class="left">调整此任务节点等级，子任务跟着变化</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c -</td>
<td class="left">更换列表标记(循环)</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-S-RET</td>
<td class="left">新增一个子项</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-up/M-down</td>
<td class="left">调整此任务节点的顺序</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">Tables</td>
<td class="left">C-c -</td>
<td class="left">在下面添加水平线</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c RET</td>
<td class="left">添加水平线并跳转到下一行</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-m</td>
<td class="left">在本列下面添加一行</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-S-Right</td>
<td class="left">在本列后面添加一列</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-S-Down</td>
<td class="left">在本行上面添加一行</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-S-Left</td>
<td class="left">删除本列</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-S-UP</td>
<td class="left">删除本行</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-left/right</td>
<td class="left">移动列</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-Up/Down</td>
<td class="left">移动行</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">Emacs-edit</td>
<td class="left">C-c d</td>
<td class="left">复制当前行</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c c</td>
<td class="left">注释/取消注释</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-RET</td>
<td class="left">下面新建一行并自动缩进</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-o</td>
<td class="left">新建一行并自动缩进，但光标不变化</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-S-down</td>
<td class="left">往下移动行</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-%</td>
<td class="left">查找替换, y替换，n不替换，q退出，！替换后面所有</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-k</td>
<td class="left">删除光标后面的内容，html模式中可直接删除整个tag集</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-S-k</td>
<td class="left">不管光标在哪，删除此行且光标移动到缩进首</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c l</td>
<td class="left">复制当前行，不用选择也不用移动到行首</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-h</td>
<td class="left">删除已经选择的内容，删除内容</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c h</td>
<td class="left">全选</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-S-i</td>
<td class="left">缩进已经选择的或当前行</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-S n/p/b/f</td>
<td class="left">一次性移动5格</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c b</td>
<td class="left">切换Buffer</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-s</td>
<td class="left">往后搜索</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-r</td>
<td class="left">往前搜索</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-e</td>
<td class="left">去到行尾</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-up/down</td>
<td class="left">html模式当中, 按标签对上下移动</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-j</td>
<td class="left">将上一行缩进到本行后面</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-a</td>
<td class="left">返回到行首</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-i</td>
<td class="left">返回到本行的缩进位置</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-l</td>
<td class="left">调整当前光标所在行为屏幕最上面或中间或最下面</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-;</td>
<td class="left">当前系统剪贴版</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-></td>
<td class="left">向下选择多个光标</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-<</td>
<td class="left">向上选择多个光标</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-></td>
<td class="left">跳转到页面最后</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-u</td>
<td class="left">大写转换</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-q</td>
<td class="left">对长的行进行自动折行处理</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-<</td>
<td class="left">跳转到文件最头</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-n/p</td>
<td class="left">跳转块,跳转到下一个空行；</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-h</td>
<td class="left">删除退格键</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-y</td>
<td class="left">粘贴内容</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-x C-y</td>
<td class="left">选择性粘贴内容,打开剪贴板</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-M-,</td>
<td class="left">将当前行设置一个mark，可以通过C-M-<进行退回</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-M-<</td>
<td class="left">退回到上一个mark的行，用于快速返回</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-M-></td>
<td class="left">取消所有的mark，用于对mark进行初使化</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c C-f</td>
<td class="left">Go to next line and make the point at the end of this line</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c C-b</td>
<td class="left">Back to above line make the point at the end o fthis liner</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-S 鼠标点击</td>
<td class="left">通过鼠标点击选择多个光标</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">Emacs operation</td>
<td class="left">C-x r q</td>
<td class="left">快速退出emacs</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-x C-c</td>
<td class="left">退出emacs标准版</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c C-s</td>
<td class="left">保存当前文件</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-[</td>
<td class="left">扩大当前窗口</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-]</td>
<td class="left">缩小当前窗口</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-x C-+</td>
<td class="left">放大当前buffer字体</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-x C-0</td>
<td class="left">返回原来buffer字体大小（zoom-frm-in/out可以对整个frm的字体进行放大缩小，zoom-in/out功能相同）</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-x C--</td>
<td class="left">切换当前窗口内buffer的顺序</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-x -</td>
<td class="left">切换当前窗口之间的结构，横向切换为纵向，反之</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">Smart selection</td>
<td class="left">C-'</td>
<td class="left">智能选择区域,适用于如csc, js, html等代码模式，org模式则为打开另一个org模式文件</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">HTML-edit</td>
<td class="left">M-up/down</td>
<td class="left">Tags成对移动</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">Shell</td>
<td class="left">C-z</td>
<td class="left">打开shell-mode</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">Dired</td>
<td class="left">C-x d</td>
<td class="left">打开Dired</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">$</td>
<td class="left">隐藏/显示目录结构</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">p</td>
<td class="left">上一个文件夹/文件</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">n</td>
<td class="left">下一个文件夹/文件</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">q</td>
<td class="left">返回目录</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">o</td>
<td class="left">另一个窗口打开文件</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">g</td>
<td class="left">刷新当前目录</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">&#xa0;</td>
<td class="left">m</td>
<td class="left">标记当前文件夹/文件</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">t</td>
<td class="left">标记所有</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">u</td>
<td class="left">取消标记</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">d</td>
<td class="left">标记为删除</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">R</td>
<td class="left">重命名</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">&#xa0;</td>
<td class="left">X</td>
<td class="left">删除</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">k</td>
<td class="left">移动到回收站</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">R</td>
<td class="left">移动或重命名</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C</td>
<td class="left">复制</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">+</td>
<td class="left">新建文件夹</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-x C-f</td>
<td class="left">新建文件</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">&#xa0;</td>
<td class="left">M</td>
<td class="left">改变权限</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">O</td>
<td class="left">改变用户</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">&#xa0;</td>
<td class="left">M-g</td>
<td class="left">在marked文件上执行grep命令进行查看文件代码</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-x C-h</td>
<td class="left">显示隐藏文件（默认配置了不显示）</td>
</tr>
</tbody>
</table>

# Emacs Wanderlust邮件客户端配置<a id="sec-3" name="sec-3"></a>

P

## Wanderlust安装<a id="sec-3-1" name="sec-3-1"></a>

在Archlinux上有包的管理：因此直接通过sudo命令即可以直接安装；

    sudo pacman -S wanderlust

安装完成后，它可能会在/usr/share/emacs/site-lisp/wl下生成对应的el配置文件；
可以将配置文件拷到对应~/.emacs.d/site-list/wl下；
接着需要安装它的相应的依赖包：elmo, bbdb, semi, w3m, 如果使用ssl协议还需要下载ssl.el文件；
这些包可以在一些相应的网站可以下载：如<http://www.emacswiki.org/上下载>；

## Wanderlust配置<a id="sec-3-2" name="sec-3-2"></a>

下载我的配置文件到你的文件夹中：<https://github.com/lujianmei/.emacs.d>；
找到~/.emacs.d/user/kevin/init-wl.el~文件下载；
-   配置用户名，目录等相关信息
    -   按文件中的目录信息配置相关的存储目录，默认为~/mails，如果不需要修改则可以不用修改；
-   配置imap目录，与邮箱web端的目录结构相同
    -   参照~/.emacs.d/user/kevin/folders文件，配置与生产环境相同的目录结构；
    -   可以直接通过邮箱服务端配置好邮件的过滤功能，然后直接在wl中进行下载查看；
-   配置登录密码，邮件发送密码
    -   init-wl.el文件中已经默认配置好了passwd的文件目录，用于存储imap, stmp的加密后的密码信息的；
    -   因此可以将folders文件按要求放到对应的目录下，然后在登录邮箱，并成功发送邮件后，执行：M-x elmo-passwd-alist-save方法，它即会自动将密码信息写入此文件中；
    -   下次即不再要求通过密码校验了；
-   配置签名文件
    -   可以在init-wl.el文件中找到对应的signature文件的目录，因此在对应的地方新建一个文件，然后将签名内容拷进去；
    -   则可以在发送邮件时自动生成对应的签名在后面；

## Wanderlust快捷键<a id="sec-3-3" name="sec-3-3"></a>

-   快捷键： 查看官方文档：<http://www.gohome.org/wl/doc/wl_toc.html>；

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<tbody>
<tr>
<td class="left">键位</td>
<td class="left">功能描述</td>
</tr>


<tr>
<td class="left">l</td>
<td class="left">打开/关闭左边的目录导航</td>
</tr>


<tr>
<td class="left">a</td>
<td class="left">在进入邮件内容后，可以直接进行回复</td>
</tr>


<tr>
<td class="left">SPAC/RET</td>
<td class="left">查看邮件内容</td>
</tr>


<tr>
<td class="left">n</td>
<td class="left">查看下一条邮件</td>
</tr>


<tr>
<td class="left">p</td>
<td class="left">查看上一条邮件</td>
</tr>


<tr>
<td class="left">S-n</td>
<td class="left">查看下一条未查看邮件</td>
</tr>


<tr>
<td class="left">S-p</td>
<td class="left">查看上一条未查看邮件</td>
</tr>


<tr>
<td class="left">S-s</td>
<td class="left">按字段进行邮件排序</td>
</tr>
</tbody>
</table>
