<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-0-1">0.1. org-mode configuration</a></li>
<li><a href="#sec-0-2">0.2. Shot-key</a>
<ul>
<li><a href="#sec-0-2-1">0.2.1. Base B</a></li>
<li><a href="#sec-0-2-2">0.2.2. Dired</a></li>
<li><a href="#sec-0-2-3">0.2.3. Tern</a></li>
<li><a href="#sec-0-2-4">0.2.4. Wanderlust</a></li>
<li><a href="#sec-0-2-5">0.2.5. Projectile &amp; helm</a></li>
<li><a href="#sec-0-2-6">0.2.6. Projectile</a></li>
<li><a href="#sec-0-2-7">0.2.7. SMIX</a></li>
</ul>
</li>
<li><a href="#sec-0-3">0.3. Emacs Wanderlust邮件客户端配置</a>
<ul>
<li><a href="#sec-0-3-1">0.3.1. Wanderlust Install</a></li>
<li><a href="#sec-0-3-2">0.3.2. Wanderlust Configuration</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#sec-1">1. 包管理</a>
<ul>
<li>
<ul>
<li><a href="#sec-1-0-1">1.0.1. 已安装的包</a></li>
<li><a href="#sec-1-0-2">1.0.2. 需要重点学习的包</a></li>
</ul>
</li>
</ul>
</div>
</div>

\#+DOCUMENT 其它文档

## org-mode configuration<a id="sec-0-1" name="sec-0-1"></a>

## Shot-key<a id="sec-0-2" name="sec-0-2"></a>

### Base B<a id="sec-0-2-1" name="sec-0-2-1"></a>

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
<td class="left">M-w</td>
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
<td class="left">C-x C-w</td>
<td class="left">另存为</td>
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
<td class="left">C-x b</td>
<td class="left">切换文件</td>
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
<td class="left">Eproject(不再使用)</td>
<td class="left">C-x p n</td>
<td class="left">打开项目及工作记录文档</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-x p a</td>
<td class="left">打开移动前端框架项目</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-x p f</td>
<td class="left">打开前端框架项目</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-x p o</td>
<td class="left">打开项目中所有文档</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-x p v</td>
<td class="left">打开项目</td>
</tr>
</tbody>
</table>

### Dired<a id="sec-0-2-2" name="sec-0-2-2"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
<caption class="t-above"><span class="table-number">Table 2:</span> Dired快捷键</caption>

<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">类别</th>
<th scope="col" class="left">快捷键</th>
<th scope="col" class="left">描述</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">基本</td>
<td class="left">C-x d</td>
<td class="left">启动dired</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">;</td>
<td class="left">切换View-mode与Dired-mode，View-mode可以通过首字母定位文件名，Dired-mode下可以使用快捷键</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">Dired-mode</td>
<td class="left">n/p</td>
<td class="left">上一个，下一个</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
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

### Tern<a id="sec-0-2-3" name="sec-0-2-3"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
<caption class="t-above"><span class="table-number">Table 3:</span> Tern快捷键</caption>

<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">快捷键</th>
<th scope="col" class="left">描述</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">M-.</td>
<td class="left">跳转到当前所在的参数或方法的定义位置</td>
</tr>


<tr>
<td class="left">M-,</td>
<td class="left">返回刚在执行M-.的位置</td>
</tr>


<tr>
<td class="left">C-c C-c</td>
<td class="left">重命名当前变量</td>
</tr>


<tr>
<td class="left">C-c C-d</td>
<td class="left">找到当前变量的文档，再按就是打开它的文档中的URL</td>
</tr>


<tr>
<td class="left">C-<tab></td>
<td class="left">自动提示</td>
</tr>
</tbody>
</table>

### Wanderlust<a id="sec-0-2-4" name="sec-0-2-4"></a>

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

### Projectile & helm<a id="sec-0-2-5" name="sec-0-2-5"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
<caption class="t-above"><span class="table-number">Table 4:</span> 绑定helm后的快捷键</caption>

<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">分类</th>
<th scope="col" class="left">快捷键</th>
<th scope="col" class="left">描述</th>
<th scope="col" class="left">掌握重点</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">基本查找</td>
<td class="left">C-c p h</td>
<td class="left">打开helm-projectile，查看当前管理的项目列表</td>
<td class="left">常用</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c p p</td>
<td class="left">当配置helm直接接管projectile后，可以直接用projectile项目切换快捷键</td>
<td class="left">常用</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-d</td>
<td class="left">使用Dired打开项目地址目录</td>
<td class="left">常用</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-g</td>
<td class="left">打开项目root目录</td>
<td class="left">常用</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-e</td>
<td class="left">在项目中打开Eshell</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-s</td>
<td class="left">使用grep命令</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-u C-s</td>
<td class="left">使用grep进行递归查找</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c</td>
<td class="left">执行编译命令（可配置）</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c p d</td>
<td class="left">查找项目中的文件夹</td>
<td class="left">常用</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c p e</td>
<td class="left">打开近期打开的文件</td>
<td class="left">常用</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c p a</td>
<td class="left">打开当前名称相同的另一个后缀不相同的文件（js/css名称相同时用）</td>
<td class="left">常用</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c p i</td>
<td class="left">刷新项目文件缓存</td>
<td class="left">有时</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c p z</td>
<td class="left">将当前文件添加到项目中</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-D</td>
<td class="left">删除项目</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">文件管理</td>
<td class="left">C-c p f</td>
<td class="left">在项目中查找文件</td>
<td class="left">常用</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c o</td>
<td class="left">在新窗口中打开文件</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-SPC</td>
<td class="left">标注</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c r</td>
<td class="left">用root打开文件</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-R</td>
<td class="left">对文件进行重命名</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-C</td>
<td class="left">拷贝文件</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">M-D</td>
<td class="left">删除文件</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c p g</td>
<td class="left">重新匹配输入的命令，用于在未发现文件时的操作</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">缓冲管理</td>
<td class="left">C-c p b</td>
<td class="left">在项目中切换buffer</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">项目搜索</td>
<td class="left">C-c p s g</td>
<td class="left">项目中搜索内容</td>
<td class="left">常用</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c p s a</td>
<td class="left">使用ack搜索内容</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">C-c p s s</td>
<td class="left">使用ag搜索内容</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

### Projectile<a id="sec-0-2-6" name="sec-0-2-6"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
<caption class="t-above"><span class="table-number">Table 5:</span> Projectile快捷键收集</caption>

<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<tbody>
<tr>
<td class="left">快捷键</td>
<td class="left">描述</td>
</tr>


<tr>
<td class="left">C-c p f</td>
<td class="left">Display a list of all files in the project. With a prefix argument it will clear the cache first.</td>
</tr>


<tr>
<td class="left">C-c p F</td>
<td class="left">Display a list of all files in all known projects.</td>
</tr>


<tr>
<td class="left">C-c p g</td>
<td class="left">Display a list of all files at point in the project. With a prefix argument it will clear the cache first.</td>
</tr>


<tr>
<td class="left">C-c p 4 f</td>
<td class="left">Jump to a project's file using completion and show it in another window.</td>
</tr>


<tr>
<td class="left">C-c p 4 g</td>
<td class="left">Jump to a project's file based on context at point and show it in another window.</td>
</tr>


<tr>
<td class="left">C-c p d</td>
<td class="left">Display a list of all directories in the project. With a prefix argument it will clear the cache first.</td>
</tr>


<tr>
<td class="left">C-c p 4 d</td>
<td class="left">Switch to a project directory and show it in another window.</td>
</tr>


<tr>
<td class="left">C-c p 4 a</td>
<td class="left">Switch between files with the same name but different extensions in other window.</td>
</tr>


<tr>
<td class="left">C-c p T</td>
<td class="left">Display a list of all test files(specs, features, etc) in the project.</td>
</tr>


<tr>
<td class="left">C-c p l</td>
<td class="left">Display a list of all files in a directory (that's not necessarily a project)</td>
</tr>


<tr>
<td class="left">C-c p s g</td>
<td class="left">Run grep on the files in the project.</td>
</tr>


<tr>
<td class="left">M&#x2013; C-c p s g</td>
<td class="left">Run grep on projectile-grep-default-files in the project.</td>
</tr>


<tr>
<td class="left">C-c p v</td>
<td class="left">Run vc-dir on the root directory of the project.</td>
</tr>


<tr>
<td class="left">C-c p b</td>
<td class="left">Display a list of all project buffers currently open.</td>
</tr>


<tr>
<td class="left">C-c p 4 b</td>
<td class="left">Switch to a project buffer and show it in another window.</td>
</tr>


<tr>
<td class="left">C-c p 4 C-o</td>
<td class="left">Display a project buffer in another window without selecting it.</td>
</tr>


<tr>
<td class="left">C-c p a</td>
<td class="left">Switch between files with the same name but different extensions.</td>
</tr>


<tr>
<td class="left">C-c p o</td>
<td class="left">Runs multi-occur on all project buffers currently open.</td>
</tr>


<tr>
<td class="left">C-c p r</td>
<td class="left">Runs interactive query-replace on all files in the projects.</td>
</tr>


<tr>
<td class="left">C-c p i</td>
<td class="left">Invalidates the project cache (if existing).</td>
</tr>


<tr>
<td class="left">C-c p R</td>
<td class="left">Regenerates the projects TAGS file.</td>
</tr>


<tr>
<td class="left">C-c p j</td>
<td class="left">Find tag in project's TAGS file.</td>
</tr>


<tr>
<td class="left">C-c p k</td>
<td class="left">Kills all project buffers.</td>
</tr>


<tr>
<td class="left">C-c p D</td>
<td class="left">Opens the root of the project in dired.</td>
</tr>


<tr>
<td class="left">C-c p e</td>
<td class="left">Shows a list of recently visited project files.</td>
</tr>


<tr>
<td class="left">C-c p s s</td>
<td class="left">Runs ag on the project. Requires the presence of ag.el.</td>
</tr>


<tr>
<td class="left">C-c p !</td>
<td class="left">Runs shell-command in the root directory of the project.</td>
</tr>


<tr>
<td class="left">C-c p &</td>
<td class="left">Runs async-shell-command in the root directory of the project.</td>
</tr>


<tr>
<td class="left">C-c p c</td>
<td class="left">Runs a standard compilation command for your type of project.</td>
</tr>


<tr>
<td class="left">C-c p P</td>
<td class="left">Runs a standard test command for your type of project.</td>
</tr>


<tr>
<td class="left">C-c p t</td>
<td class="left">Toggle between an implementation file and its test file.</td>
</tr>


<tr>
<td class="left">C-c p 4 t</td>
<td class="left">Jump to implementation or test file in other window.</td>
</tr>


<tr>
<td class="left">C-c p z</td>
<td class="left">Adds the currently visited file to the cache.</td>
</tr>


<tr>
<td class="left">C-c p p</td>
<td class="left">Display a list of known projects you can switch to.</td>
</tr>


<tr>
<td class="left">C-c p S</td>
<td class="left">Save all project buffers.</td>
</tr>


<tr>
<td class="left">C-c p m</td>
<td class="left">Run the commander (an interface to run commands with a single key).</td>
</tr>


<tr>
<td class="left">C-c p ESC</td>
<td class="left">Switch to the most recently selected projectile buffer.</td>
</tr>
</tbody>
</table>

### SMIX<a id="sec-0-2-7" name="sec-0-2-7"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
<caption class="t-above"><span class="table-number">Table 6:</span> SMIX快捷键</caption>

<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<tbody>
<tr>
<td class="left">类别</td>
<td class="left">快捷键</td>
<td class="left">描述</td>
</tr>


<tr>
<td class="left">SMIX</td>
<td class="left">M-x</td>
<td class="left">打开SMIX</td>
</tr>
</tbody>
</table>

## Emacs Wanderlust邮件客户端配置<a id="sec-0-3" name="sec-0-3"></a>

### Wanderlust Install<a id="sec-0-3-1" name="sec-0-3-1"></a>

在Archlinux上有包的管理：因此直接通过sudo命令即可以直接安装；

    sudo pacman -S wanderlust

安装完成后，它可能会在/usr/share/emacs/site-lisp/wl下生成对应的el配置文件；
可以将配置文件拷到对应~/.emacs.d/site-list/wl下；
接着需要安装它的相应的依赖包：elmo, bbdb, semi, w3m, 如果使用ssl协议还需要下载ssl.el文件；
这些包可以在一些相应的网站可以下载：如<http://www.emacswiki.org/上下载>；

### Wanderlust Configuration<a id="sec-0-3-2" name="sec-0-3-2"></a>

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

# 包管理<a id="sec-1" name="sec-1"></a>

### 已安装的包<a id="sec-1-0-1" name="sec-1-0-1"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Package name</th>
<th scope="col" class="left">Markdown</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">Projectile</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">helm-projectile</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">project-codesearch</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">helm</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">wanderlust</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">anything</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">dired+</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">dired-details</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">dired-details+</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">dired-sort</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">expand-region</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">js2-refactor</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">jump-char</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">multifiles</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">multiple-cursors</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">paredit</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">perspective</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">skewer</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">smart-forward</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">smex</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">yasnippet</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">zencoding-mode</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">codesearch</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">boxquote</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">magit</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">simple-httpd</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">height-symbol</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">ido-completing-read+</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">ox-twbs</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">tern</td>
<td class="left"><http://ternjs.net></td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

### 需要重点学习的包<a id="sec-1-0-2" name="sec-1-0-2"></a>

1.  Projectile

2.  Helm

3.  Smex

4.  Wanderlust

5.  Org-mode

6.  Markdown

7.  Ido

8.  zencoding-mode

9.  yasnippet

10. dired

11. anything

12. wgrep

13. skewer

14. tern/tern-server
