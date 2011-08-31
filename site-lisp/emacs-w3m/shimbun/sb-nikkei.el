;;; sb-nikkei.el --- shimbun backend for nikkei.co.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2009
;; Kazuyoshi KOREEDA <Koreeda.Kazuyoshi@jp.panasonic.com>

;; Author: Kazuyoshi KOREEDA <Koreeda.Kazuyoshi@jp.panasonic.com>,
;;         Katsumi Yamaoka   <yamaoka@jpl.org>,
;;         NOMIYA Masaru     <nomiya@ttmy.ne.jp>
;; Keywords: news

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it a>nd/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Original code was sb-asahi.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>.

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-nikkei (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-nikkei-top-level-domain "nikkei.co.jp"
  "Name of the top level domain for the Nikkei Net.")

(defvar shimbun-nikkei-url
  (concat "http://www." shimbun-nikkei-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-nikkei-url-coding-system 'shift_jis
  "Coding system used to encode URLs containing non-ASCII letters.")

(defvar shimbun-nikkei-group-table
  `(("top" "トップ" ,shimbun-nikkei-url
     shimbun-nikkei-get-headers-top
     shimbun-nikkei-prepare-article-default)
    ("main" "主要" ,(concat shimbun-nikkei-url "news/main/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("keizai" "経済" ,(concat shimbun-nikkei-url "news/keizai/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("sangyo" "企業" ,(concat shimbun-nikkei-url "news/sangyo/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("tento" "ベンチャー" ,(concat shimbun-nikkei-url "news/tento/")
     shimbun-nikkei-get-headers-default2
     shimbun-nikkei-prepare-article-default2)
    ("kansai" "関西" ,(concat shimbun-nikkei-url "kansai/")
     shimbun-nikkei-get-headers-kansai
     shimbun-nikkei-prepare-article-kansai)
    ("it.business" "ITビジネス"
     "http://it.nikkei.co.jp/business/news/index.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.busi_gyoukai" "業界動向(ITビジネス)"
     "http://it.nikkei.co.jp/business/news/busi_gyoukai.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.biz-system" "企業情報システム"
     "http://it.nikkei.co.jp/business/news/busi_system.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.sox" "盛り上がるＳＯＸ法ビジネス"
     "http://it.nikkei.co.jp/business/special/sox.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.data" "データで読むＩＴ市場"
     "http://it.nikkei.co.jp/business/column/data.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.taidan" "トップ対談"
     "http://it.nikkei.co.jp/business/column/taidan.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.internet" "インターネット"
     "http://it.nikkei.co.jp/internet/news/index.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.broad" "ブロードバンド"
     "http://it.nikkei.co.jp/internet/news/broadband.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.net_gyoukai" "業界動向(インターネット)"
     "http://it.nikkei.co.jp/internet/news/net_gyoukai.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.iptel" "多機能化するＩＰ電話"
     "http://it.nikkei.co.jp/internet/special/iptel.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.tele" "放送・ネット融合"
     "http://it.nikkei.co.jp/internet/special/tele.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.broadcast" "地上デジタル放送"
     "http://it.nikkei.co.jp/internet/special/d_broadcast.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.internet-column" "インターネット:コラム"
     "http://it.nikkei.co.jp/internet/column/koike.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.contents" "コンテンツビジネス"
     "http://it.nikkei.co.jp/internet/column/contents.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.ec" "EC"
     "http://it.nikkei.co.jp/internet/news/ec.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.policy" "政策・統計"
     "http://it.nikkei.co.jp/internet/news/seisaku.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.e-gov" "行政のＩＴ化"
     "http://it.nikkei.co.jp/business/special/e-gov.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.mobile" "モバイル"
     "http://it.nikkei.co.jp/mobile/news/index.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.mob_gyoukai" "業界動向(モバイル)"
     "http://it.nikkei.co.jp/mobile/news/gyoukai.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.mobsoft" "サービス"
     "http://it.nikkei.co.jp/mobile/news/soft.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.mobcon" "コンテンツ"
     "http://it.nikkei.co.jp/mobile/news/contents.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.money" "携帯キャリアの金融ビジネス"
     "http://it.nikkei.co.jp/mobile/special/money.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.one" "ワンセグはテレビを変えるか"
     "http://it.nikkei.co.jp/mobile/special/one.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.security" "セキュリティ"
     "http://it.nikkei.co.jp/security/news/index.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.net_crime" "ネット犯罪"
     "http://it.nikkei.co.jp/security/news/net_crime.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.digital" "デジタル家電＆エンタメ"
     "http://it.nikkei.co.jp/digital/news/index.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.pc" "PC＆デジタルカメラ"
     "http://it.nikkei.co.jp/pc/news/index.aspx"
     shimbun-nikkei-get-headers-it-pc
     shimbun-nikkei-prepare-article-default2)
    ("kokunai" "市場概況" "http://markets.nikkei.co.jp/kokunai/summary.aspx"
     shimbun-nikkei-get-headers-stock
     shimbun-nikkei-prepare-article-default4)
    ("markets" "海外株概況" "http://markets.nikkei.co.jp/kaigai/summary.aspx"
     shimbun-nikkei-get-headers-stock
     shimbun-nikkei-prepare-article-default4)
    ("kawase" "為替概況" "http://markets.nikkei.co.jp/kawase/summary.aspx"
     shimbun-nikkei-get-headers-stock
     shimbun-nikkei-prepare-article-default4)
    ("kinri" "短期金利・債権・ＣＢ概況"
     "http://markets.nikkei.co.jp/kawase/kinri.aspx"
     shimbun-nikkei-get-headers-stock
     shimbun-nikkei-prepare-article-default4)
    ("ft" "英フィナンシャル・タイムズ"
     "http://markets.nikkei.co.jp/kaigai/ft.aspx"
     shimbun-nikkei-get-headers-ft
     shimbun-nikkei-prepare-article-default4)
    ("dj" "米ダウ・ジョーンズ" "http://markets.nikkei.co.jp/kaigai/dj.aspx"
     shimbun-nikkei-get-headers-dj
     shimbun-nikkei-prepare-article-default4)
    ("ngyoseki" "企業業績ニュース"
     "http://markets.nikkei.co.jp/kokunai/gyoseki.aspx"
     shimbun-nikkei-get-headers-stock
     shimbun-nikkei-prepare-article-default4)
    ("gyosuuchi" "業績数値"
     "http://markets.nikkei.co.jp/kokunai/bunkatsu2.aspx?genre=m4"
     shimbun-nikkei-get-headers-gyosuuchi
     shimbun-nikkei-prepare-article-default4)
    ("gyoseki" "海外企業業績" "http://markets.nikkei.co.jp/kaigai/gyoseki.aspx"
     shimbun-nikkei-get-headers-stock
     shimbun-nikkei-prepare-article-default4)
    ("china" "中国ビジネス事情" ,(concat shimbun-nikkei-url "china/news/")
     shimbun-nikkei-get-headers-china
     shimbun-nikkei-prepare-article-default4)
    ("market" "株・為替" ,(concat shimbun-nikkei-url "news/market/")
     shimbun-nikkei-get-headers-market
     shimbun-nikkei-prepare-article-market)
    ("kaigai" "国際" ,(concat shimbun-nikkei-url "news/kaigai/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("seiji" "政治" ,(concat shimbun-nikkei-url "news/seiji/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("shakai" "社会" ,(concat shimbun-nikkei-url "news/shakai/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("retto" "地域経済" ,(concat shimbun-nikkei-url "news/retto/")
     shimbun-nikkei-get-headers-retto
     shimbun-nikkei-prepare-article-default4)
    ("sports" "スポーツ" "http://sports.nikkei.co.jp/"
     shimbun-nikkei-get-headers-sports
     shimbun-nikkei-prepare-article-sports)
    ("newpro" "新製品" ,(concat shimbun-nikkei-url "newpro/news/")
     shimbun-nikkei-get-headers-newpro
     shimbun-nikkei-prepare-article-newpro)
    ("release" "プレスリリース" "http://release.nikkei.co.jp/"
     shimbun-nikkei-get-headers-release
     shimbun-nikkei-prepare-article-release)
    ("release.it.comp" "プレスリリース(ＩＴ；コンピューター)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=1"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.peri" "プレスリリース(ＩＴ；周辺機器)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=2"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.sys" "プレスリリース(ＩＴ；システム・ソフト開発)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=3"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.cont" "プレスリリース(ＩＴ；情報・コンテンツ)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=4"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.net" "プレスリリース(ＩＴ；通信・インターネット)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=5"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.lsi" "プレスリリース(ＩＴ；半導体)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=6"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.game" "プレスリリース(ＩＴ；ゲーム・娯楽)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=7"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.etc" "プレスリリース(ＩＴ；その他ＩＴ関連)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=8"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.depart" "プレスリリース(流通；百貨店・スーパー)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=9"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.ryohan" "プレスリリース(流通；量販店)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=10"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.zakka" "プレスリリース(流通；生活雑貨)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=11"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.cosme" "プレスリリース(流通；医薬品・化粧品)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=12"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.car" "プレスリリース(流通；自動車)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=13"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.book" "プレスリリース(流通；書籍)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=14"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.record" "プレスリリース(流通；レコード・ゲーム)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=15"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.food" "プレスリリース(流通；食品・飲料)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=16"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.mercha" "プレスリリース(流通；商社・卸売業)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=17"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.mail" "プレスリリース(流通；通信・訪問販売)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=18"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.netshop" "プレスリリース(流通；ネットショッピング)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=19"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.etc" "プレスリリース(流通；その他商業)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=20"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.bank" "プレスリリース(金融；銀行・信金)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=57"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.sec" "プレスリリース(金融；証券会社)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=58"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.am" "プレスリリース(金融；投資信託運用会社)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=59"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.insu" "プレスリリース(金融；保険会社)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=60"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.etc" "プレスリリース(金融；その他金融)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=61"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.chemi" "プレスリリース(メーカー；化学・医薬品)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=31"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.mecha" "プレスリリース(メーカー；機械・金属)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=32"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.car" "プレスリリース(メーカー；自動車・自動車部品)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=33"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.elec" "プレスリリース(メーカー；家電・電機)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=34"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.food" "プレスリリース(メーカー；食品・飲料)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=35"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.sports" "プレスリリース(メーカー；スポーツ・娯楽用品)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=36"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.apparel" "プレスリリース(メーカー；アパレル・生活用品)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=37"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.commu" "プレスリリース(メーカー；通信機器・精密機械)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=38"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.etc" "プレスリリース(メーカー；その他メーカー)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=39"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.medic" "プレスリリース(サービス；医療・福祉)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=40"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.rest" "プレスリリース(サービス；飲食)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=41"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.trans" "プレスリリース(サービス；運輸・運送)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=42"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.energy" "プレスリリース(サービス；エネルギー)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=43"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.enter" "プレスリリース(サービス；エンターテインメント)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=44"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.env" "プレスリリース(サービス；環境)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=45"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.consul" "プレスリリース(サービス；コンサルティング)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=46"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.edu" "プレスリリース(サービス；教育・研修)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=47"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.haken" "プレスリリース(サービス；人材派遣)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=48"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.life" "プレスリリース(サービス；生活関連)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=49"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.media" "プレスリリース(サービス；メディア)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=50"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.lease" "プレスリリース(サービス；リース)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=51"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.travel" "プレスリリース(サービス；旅行・ホテル)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=52"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.etc" "プレスリリース(サービス；その他サービス業)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=53"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.const.const" "プレスリリース(サービス；建設・土木)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=6&sindID=54"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.const.house" "プレスリリース(サービス；住宅)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=6&sindID=56"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.const.etc" "プレスリリース(サービス；その他建設関連)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=53"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("shasetsu" "社説・春秋"
     "http://www.nikkei.co.jp/news/shasetsu/childKijiIchiran.js"
     ;; The contents of IndexKijiIchiran.js will be appended afterward.
     shimbun-nikkei-get-headers-shasetsu
     shimbun-nikkei-prepare-article-default))
  "Alist of group names and parameters.
Each parameters include a Japanese group name, an index page, a
function used to get headers and a function used to prepare an article.")

(defvar shimbun-nikkei-server-name "日本経済新聞")
(defvar shimbun-nikkei-from-address "nobody@example.com")
(defvar shimbun-nikkei-content-start
  "<!--emacs-w3m-shimbun-nikkei-content-start-->")
(defvar shimbun-nikkei-content-end
  "<!--emacs-w3m-shimbun-nikkei-content-end-->")
(defvar shimbun-nikkei-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAACAAAAAgBAMAAACBVGfHAAAABGdBTUEAALGPC/xhBQAAAA9
 QTFRF////AY8ACZYCg8p7////e1VqQQAAAAlwSFlzAAAOnAAADsQBdfaEgQAAACh0RVh0U29mdHd
 hcmUAWFYgVmVyc2lvbiAzLjEwYSAgUmV2OiAxMi8yOS85NFhtieMAAADcSURBVCjPXZHhkcUgCIS
 9vAouaUDRAqKPCnT7r+lYTGLm+LffsLADIXh9RM7wKvH6p/MiG3XSdhNqExi7xMcw5ADuMfSjFqC
 KxstR0BXoB87LUTAIFH2CZIoABNsCXwNxhrCJDcg3SL+2s6rNRIvBIuVv08GkjBIYgoZU6oHmwLR
 tHWptOwEbfAOjEhQHTMoGCVUn2IHkV/qZYKTS2RAfIKWLg+2yJL1AmENb0jkzhLlW5AHuMWFjZB4
 1q+24bhrXl7IHXW8RP3Jcj+HRurxfycnn67m+/PX+j976D3rKSsvKwOwqAAAAB3RJTUUH1wgDChM
 rIe5fHAAAAABJRU5ErkJggg==")
    ("\\`release" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAWAgMAAAD7mfc/AAAABGdBTUEAALGPC/xhBQAAAAx
 QTFRFBjKeZ4rcxdHp+/z7lhoK9wAAAC90RVh0U29mdHdhcmUAWFYgVmVyc2lvbiAzLjEwYStGTG1
 hc2sgIFJldjogMTIvMjkvOTQbx6p8AAABBUlEQVR4nD3MIU/DQBwF8D8d62W9wGqalYRSREUV9xX
 aC0snKzgQC8GgmUFg2Lh+AhD11dUYEsj2Ea6i/swEbqKiTZYeHIJnXn7iPehwXRUfs7tpbL5Biyo
 uZOiZYHJoUMb5y3twhhSFnZHR2NjZz7hHIGkyiJFMP8FCILhpIiqKp+EthkqMLMQ3TUD2Y8jEyQi
 LuLFJ6wMVrjsWRgvOPYFEBqEvjSuAcwJK55uVuv7Qs7n6xzYlSWrYoNHn6YV1cJ06Gh2LvCOs5Eo
 jZ9Gph5VYg57fTN0Q/I1Gx+bDw9BZcP22ZQ8WPBKVadTs6xiKlaIaOdv70SssB7/oy7JbxPXlcqJ
 +AFOYhEr5ENrbAAAAB3RJTUUH1AQGFzot7I86fAAAAABJRU5ErkJggg==")))

(defvar shimbun-nikkei-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-nikkei))
  (mapcar 'car shimbun-nikkei-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-nikkei))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nikkei-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-nikkei))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nikkei-group-table)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-nikkei)
					 &optional range)
  (let* ((case-fold-search t)
	 (group (shimbun-current-group-internal shimbun))
	 (fn (nth 3 (assoc group shimbun-nikkei-group-table)))
	 (shimbun-nikkei-from-address
	  (concat (shimbun-server-name shimbun)
		  " (" (shimbun-current-group-name shimbun) ")"))
	 (folder (nth 2 (assoc group shimbun-nikkei-group-table))))
    (when (and (not (string-match "\\`http://markets\\.nikkei\\.co\\.jp/"
				  folder))
	       (or (member group '("kaigai" "seiji"))
		   (not (eq (aref folder (1- (length folder))) ?/)))
	       (string-match "[^/]/[^/]\\|[^/]/\\'" folder))
      (setq folder (substring folder 0 (+ (match-beginning 0) 2))))
    (while (search-forward "\r" nil t)
      (delete-backward-char 1))
    (goto-char (point-min))
    (when (fboundp fn)
      (shimbun-sort-headers (funcall fn group folder shimbun range)))))

(defun shimbun-nikkei-expand-url (url folder)
  "Make a fullname of URL relative to FOLDER.
If URL begins with `http://', FOLDER is ignored."
  (save-match-data
    (cond ((string-match "=http://" url)
	   (substring url (1+ (match-beginning 0))))
	  ((string-match "\\`http://" url)
	   url)
	  ((string-match "\\`/" url)
	   (shimbun-expand-url url shimbun-nikkei-url))
	  (t
	   (concat folder url)))))

(defun shimbun-nikkei-make-date-string (year month day
					     &optional time timezone adj)
  "Run `shimbun-make-date-string' and fix a day if needed.
Optional ADJ is the number of days that adjust the day of month."
  (save-match-data
    (let ((ctime (current-time))
	  (date (shimbun-make-date-string year month day time timezone))
	  (system-time-locale "C")
	  ms ls)
      (setq time (shimbun-time-parse-string date)
	    ms (car time)
	    ls (cadr time))
      (when adj
	(setq ls (+ ls (* adj (eval-when-compile (* 60 60 24))))
	      ms (+ (/ ls 65536) ms)
	      ls (mod ls 65536)))
      (cond ((or (> ms (car ctime))
		 (and (= ms (car ctime))
		      (> ls (cadr ctime))))
	     ;; It should be the same time yesterday.
	     (setq ms (1- ms))
	     (when (< (setq ls (- ls (eval-when-compile
				       (- (* 60 60 24) 65536))))
		      0)
	       (setq ms (1- ms)
		     ls (+ ls 65536)))
	     (format-time-string "%a, %d %b %Y %R +0900" (list ms ls)))
	    (adj
	     (format-time-string "%a, %d %b %Y %R +0900" (list ms ls)))
	    (t
	     date)))))

(defvar shimbun-nikkei-tmp-ids nil)
(defvar shimbun-nikkei-tmp-subjects nil)

(defun shimbun-nikkei-get-headers-default (group folder shimbun range
						 &optional headers)
  "Default function used to fetch headers.
GROUP is a group name.  FOLDER is a parent url.
If HEADERS is non-nil, it is appended to newly fetched headers."
  (unless headers
    (setq shimbun-nikkei-tmp-ids nil
	  shimbun-nikkei-tmp-subjects nil))
  (let (next time url subject serial day month year category id)
    (goto-char (point-min))
    (while (re-search-forward "<span\\(?:[\t\n ]+[^>]+\\)?>\
\[\t\n ]*(\\([012][0-9]:[0-5][0-9]\\))[\t\n ]*</span>"
			      nil t)
      (setq next (match-end 0)
	    time (match-string 1))
      (when (and (re-search-backward "<a[\t\n ]+href=\"\\([^\"]+\\)\"[^>]*>\
\[\t\n ]*\\(?:<![^>]+>[\t\n ]*\\)*\\([^\n<]+\\)\\(?:[\t\n ]*<![^>]+>\\)*\
\[\t\n ]*</a>"
				     nil t)
		 (progn
		   (setq url (match-string 1)
			 subject (match-string 2))
		   (when (string-match "[\t\n ]+\\'" url)
		     (setq url (substring url 0 (match-beginning 0))))
		   (or (and (string-match "\
\\(?:/20[0-9][0-9][01][0-9][0-3][0-9]\\|/index\\.aspx\\?n=\\)\
\\(.+\\)\\([0-3][0-9]\\)\\([01][0-9]\\)\\(20[0-9][0-9]\\)" url)
			    (prog1 t
			      (setq serial (downcase (match-string 1 url))
				    day (match-string 2 url)
				    month (match-string 3 url)
				    year (match-string 4 url))))
		       (and (string-match "\
\\(20[0-9][0-9]\\)\\([01][0-9]\\)\\([0-3][0-9]\\)\\([^\n\t ./=]+\\)" url)
			    (prog1 t
			      (setq year (match-string 1 url)
				    month (match-string 2 url)
				    day (match-string 3 url)
				    serial (downcase (match-string 4 url)))))))
		 (and (string-match "\
\\`http://\\([^\t\n ./]+\\)\\.nikkei\\.co\\.jp/\\(?:\\([^\t\n /]+\\)/news/\\)?\
\\|\\`/news/\\([^\t\n /]+\\)/" url)
		      (prog1 t
			(setq category (if (match-beginning 2)
					   (concat (match-string 2 url) "."
						   (match-string 1 url))
					 (or (match-string 1 url)
					     (match-string 3 url))))))
		 (progn
		   (setq id (concat "<" year month day "."
				    (shimbun-subst-char-in-string ?  ?_ serial)
				    "%" category "." group "."
				    shimbun-nikkei-top-level-domain ">"))
		   (not (or (member id shimbun-nikkei-tmp-ids)
			    (and (>= (length subject) 16)
				 (member subject
					 shimbun-nikkei-tmp-subjects))))))
	(push id shimbun-nikkei-tmp-ids)
	(push subject shimbun-nikkei-tmp-subjects)
	(push (shimbun-create-header
	       0 subject shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(string-to-number year)
		(string-to-number month)
		(string-to-number day)
		time)
	       id "" 0 0
	       (shimbun-nikkei-expand-url url folder))
	      headers))
      (goto-char next))
    headers))

(defun shimbun-nikkei-get-headers-top (group folder shimbun range)
  "Function used to fetch headers for the `top' group."
  (setq shimbun-nikkei-tmp-ids nil)
  (setq shimbun-nikkei-tmp-subjects nil)
  (let (headers)
    ;; Get headers for the retto group.
    (while (re-search-forward "<a[\t\n ]+href=\"/\\(news/retto/\
\\(\\(20[0-9][0-9]\\)\\([01][0-9]\\)\\([0-3][0-9]\\)\\)\\([^.]+\\)\
\\.html\\)\">[\t\n ]*\\([^<]+\\):[\n\t 　]*\\([^<]+\\)" nil t)
      (push (shimbun-create-header
	     0
	     (concat "[" (match-string 7) "] " (match-string 8)) ;; subject
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5)))
	     (concat "<" (match-string 2) "." (downcase (match-string 6))
		     "%retto.top." shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    (shimbun-nikkei-get-headers-default group folder shimbun range headers)))

(defun shimbun-nikkei-get-headers-default2 (group folder shimbun range)
  "Function used to fetch headers for the tento and the zinzi groups."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(\\(?:[^\"<>]+/\\)?"
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. day
			"\\([0-3][0-9]\\)"
			"[0-9a-z]+\\)"
			"\\.html\\)"
			s0 "\"" s0 ">" s0
			;; 6. subject
			"\\([^<>]+\\)")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5)))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-kansai (group folder shimbun range)
  "Function used to fetch headers for the kansai group."
  (let ((date (if (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"topicpath[\t\n ]+updatetime\"\
\[^>]*>[\t\n ]*\\(20[0-9][0-9]\\)/\\([01]?[0-9]\\)/\\([0-3]?[0-9]\\)\
\[\t\n 　]+更新[\t\n ]*</div>"
				     nil t)
		  (prog1
		      (shimbun-make-date-string
		       (string-to-number (match-string 1))
		       (string-to-number (match-string 2))
		       (string-to-number (match-string 3)))
		    (goto-char (point-min)))
		(let ((cts (current-time-string)))
		  (format "%s, %02d %s %s 00:00 +0900"
			  (substring cts 0 3)
			  (string-to-number (substring cts 8 10))
			  (substring cts 4 7)
			  (substring cts 20)))))
	headers)
    (while (re-search-forward "<a[\t\n ]+href=\"\
\\(news/\\(news[0-9]+\\)\\.html\\)\">[\t\n ]*\\([^<]+\\)[\t\n ]*</a>"
			      nil t)
      (push (shimbun-create-header
	     0
	     (match-string 3)
	     shimbun-nikkei-from-address
	     date
	     (concat "<" (shimbun-subst-char-in-string
			  ?/ ?. (downcase (match-string 2)))
		     "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-it-default (group folder shimbun range)
  "Function used to fetch headers for the it groups."
  (let ((pages (shimbun-header-index-pages range))
	(count 0)
	sub-end id headers)
    (catch 'stop
      (while t
	(while (re-search-forward
		(eval-when-compile
		  (let ((s0 "[\t\n ]*")
			(s1 "[\t\n ]+"))
		    (concat "<a" s1 "href=\"/?"
			    ;; 1. url
			    "\\([^\"]+="
			    ;; 2. serial number
			    "\\([^\"/]+"
			    ;; 3. day
			    "\\([0-3][0-9]\\)"
			    ;; 4. month
			    "\\([01][0-9]\\)"
			    ;; 5. year
			    "\\(20[0-9][0-9]\\)"
			    "\\)\\(?:&Page="
			    ;; 6. serial number
			    "\\([0-9]+\\)"
			    "\\)?\\)"
			    s0 "\"" s0 ">"
			    "\\(?:" s0 "([01]?[0-9]/[0-3]?[0-9])\\)?" s0
			    ;; 7. subject
			    "\\([^<]+\\)"
			    s0 "</a>")))
		nil t)
	  (setq sub-end (point)
		id (concat "<" (shimbun-subst-char-in-string
				?  ?_ (downcase (match-string 2)))
			   "%" group "."
			   shimbun-nikkei-top-level-domain ">"))
	  (if (shimbun-search-id shimbun id)
	      (unless (zerop count)
		(throw 'stop nil))
	    (push (shimbun-create-header
		   0
		   (match-string 7)
		   shimbun-nikkei-from-address
		   (shimbun-nikkei-make-date-string
		    (string-to-number (match-string 5))
		    (string-to-number (match-string 4))
		    (string-to-number (match-string 3)))
		   id "" 0 0
		   (shimbun-nikkei-expand-url (match-string 1) folder))
		  headers))
	  (goto-char sub-end))
	(if (and (or (not pages)
		     (< (setq count (1+ count)) pages))
		 (re-search-forward "\
<a href=\"\\([^\"]+\\)\">&gt;&gt; 過去記事一覧</a>\
\\|<a href=\"\\([^\"]+\\)\">次へ&gt;</a>"
				    nil t))
	    (progn
	      (shimbun-retrieve-url (prog1
					(concat "\
http://it.nikkei.co.jp/" (or (match-string 1) (match-string 2)))
				      (erase-buffer))
				    t)
	      (goto-char (point-min)))
	  (throw 'stop nil))))
    headers))

(defun shimbun-nikkei-get-headers-it-pc (group folder shimbun range)
  "Function used to fetch headers for the it.pc group."
  (let (headers)
    (while (and (re-search-forward
		 (eval-when-compile
		   (let ((s0 "[\t\n ]*")
			 (s1 "[\t\n ]+"))
		     (concat "<a" s1 "href=\"/"
			     ;; 1. url
			     "\\(pc/news/index\\.aspx\\?n="
			     ;; 2. serial number
			     "\\([^\"]+"
			     ;; 3. day
			     "\\([0-3][0-9]\\)"
			     ;; 4. month
			     "\\([01][0-9]\\)"
			     ;; 5. year
			     "\\(20[0-9][0-9]\\)"
			     "\\)\\)" s0 "\"" s0 ">" s0
			     ;; 6. subject
			     "\\([^<]+\\)"
			     s0 "</a>")))
		 nil t)
		(not (string-match "\\`[\t\n 　]*\\'" (match-string 6))))
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 3)))
	     (concat "<" (shimbun-subst-char-in-string
			  ?  ?_ (downcase (match-string 2)))
		     "%" group "." shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url
	      (concat "http://it.nikkei.co.jp/" (match-string 1))
	      folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-stock (group folder shimbun range)
  "Function used to fetch headers for stock groups."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "\
href=\"http://markets\\.nikkei\\.co\\.jp/[a-z]+/[a-z0-9]+\\.aspx"
			;; 1. url
			"\\(\\?site=MARKET&genre="
			;; 2. serial number
			"\\([0-9a-z]+\\)" "&id="
			;; 3. serial number
			"\\([\t\n 0-9a-z]+[0-3][0-9][01][0-9]"
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			"\\)"
			"\\)"
			"\"" s0 ">" s0
			;; 5. subject
			"\\([^<]+\\)"
			"</a>" "\\(?:\n\\)?" "\\(?:\n\\)?" "[\t\n ]+("
			;; 6. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 7. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 8. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 9. minute
			"\\([0-5]?[0-9]\\)"
			")" s0 "\\(?:[\t\n ]+\\)?" "</li>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 5)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 6))
	      (string-to-number (match-string 7))
	      (format "%02d:%02d"
		      (string-to-number (match-string 8))
		      (string-to-number (match-string 9))))
	     (concat "<" (downcase (match-string 2)) "."
		     (downcase (save-match-data
				 (shimbun-replace-in-string (match-string 3)
							    "[\t\n ]+" ".")))
		     "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-ft (group folder shimbun range)
  "Function used to fetch headers for the ft group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"\
http://markets\\.nikkei\\.co\\.jp/kaigai/ft\\.aspx"
			;; 1. url
			"\\(\\?site=MARKET&genre=ff&id="
			;; 2. serial number
			"\\([\t\n 0-9a-z]+[0-3][0-9][01][0-9]"
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"\\)"
			"\\)"
			"\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "[(（]"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			"[)）]")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      nil nil 1)
	     (concat "<ff."
		     (downcase (save-match-data
				 (shimbun-replace-in-string (match-string 2)
							    "[\t\n ]+" ".")))
		     "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-dj (group folder shimbun range)
  "Function used to fetch headers for the dj group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"\
http://markets\\.nikkei\\.co.jp/kaigai/dj\\.aspx"
			;; 1. url
			"\\(\\?site=MARKET&genre=dj&id="
			;; 2. serial number
			"\\([0-9a-z]+"
			"[0-3][0-9][01][0-9]"
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"\\)"
			"\\)"
			"\"" s0 ">\\(?:<DJ>\\)?" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "[(（]"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			"[)）]" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6)))
	     (concat "<dj." (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-gyosuuchi (group folder shimbun range)
  "Function used to fetch headers for the gyosuuchi group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "\
href=\"http://markets\\.nikkei\\.co\\.jp/[a-z]+/[a-z0-9]+\\.aspx"
			;; 1. url
			"\\(\\?site=MARKET&genre="
			;; 2. serial number
			"\\([0-9a-z]+\\)" "&id="
			;; 3. serial number
			"\\([\t\n 0-9a-z]+[0-3][0-9][01][0-9]"
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			"\\)"
			"\\)"
			"\"" s0 ">" s0
			;; 5. subject
			"\\([^<]+\\)"
			"</a>" "\\(?:\n\\)?" "\\(?:\n\\)?" "[\t\n ]+("
			;; 6. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 7. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 8. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 9. minute
			"\\([0-5]?[0-9]\\)"
			")" s0 "\\(?:[\t\n ]+\\)?" "</li>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 5)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 6))
	      (string-to-number (match-string 7))
	      (format "%02d:%02d"
		      (string-to-number (match-string 8))
		      (string-to-number (match-string 9))))
	     (concat "<" (downcase (match-string 2)) "."
		     (downcase (save-match-data
				 (shimbun-replace-in-string (match-string 3)
							    "[\t\n ]+" ".")))
		     "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url
	      (concat "\
http://markets.nikkei.co.jp/kokunai/bunkatsu3.aspx" (match-string 1)) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-market (group folder shimbun range)
  "Function used to fetch headers for the market group."
  (let ((subregexp
	 (eval-when-compile
	   (let ((s0 "[\t\n ]*")
		 (s1 "[\t\n ]+"))
	     (concat "class=\"sub_bar\"" s0 ">" s0
		     ;; 1. subtitle
		     "\\([^\t\n <]+\\)"
		     ".+class=\"sub_bar_time\"" s0 ">" s0
		     "更新" s0 "：" s0
		     ;; 2. month
		     "\\([01]?[0-9]\\)"
		     "月"
		     ;; 3. day
		     "\\([0-3]?[0-9]\\)"
		     "日\\(?:" s1
		     ;; 4. hour:minute
		     "\\([012]?[0-9]:[0-5]?[0-9]\\)"
		     "\\)?"))))
	subdata start end subtitle month day time from year headers)
    (when (re-search-forward subregexp nil t)
      (setq subdata (copy-sequence (match-data))
	    start (point))
      (while start
	(if (re-search-forward subregexp nil t)
	    (progn
	      (setq subdata (prog1
				(copy-sequence (match-data))
			      (set-match-data subdata))
		    end (point))
	      (goto-char start))
	  (set-match-data subdata)
	  (setq end nil))
	(setq subtitle (match-string 1)
	      month (string-to-number (match-string 2))
	      day (string-to-number (match-string 3))
	      time (match-string 4))
	(setq from (shimbun-replace-in-string
		    shimbun-nikkei-from-address
		    ")" (concat "/"
				(shimbun-replace-in-string
				 subtitle "\\(&nbsp;\\)+" "")
				")")))
	(while (re-search-forward
		(eval-when-compile
		  (let ((s0 "[\t\n ]*")
			(s1 "[\t\n ]+"))
		    (concat "<a" s1 "href=\""
			    ;; 1. url
			    "\\([^\">]+/"
			    ;; 2. id
			    "\\("
			    ;; 3. year
			    "\\(20[0-9][0-9]\\)"
			    "[^.]+"
			    "\\)"
			    "\\.html\\)"
			    s0 "\"" s0 ">\\(?:" s0 "<[^>]+>\\)*" s0
			    ;; 4. subject
			    "\\([^<]+\\)"
			    s0)))
		end t)
	  (setq year (string-to-number (match-string 3)))
	  (push (shimbun-create-header
		 0
		 (match-string 4)
		 from
		 (shimbun-nikkei-make-date-string year month day time)
		 (format "<%s%%%s.%s>"
			 (downcase (match-string 2)) group
			 shimbun-nikkei-top-level-domain)
		 "" 0 0
		 (shimbun-nikkei-expand-url (match-string 1)
					    shimbun-nikkei-url))
		headers))
	(setq start end))
      headers)))

(defun shimbun-nikkei-get-headers-china (group folder shimbun range)
  "Function used to fetch headers for the china group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\("
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. day
			"\\([0-3][0-9]\\)"
			"[0-9_a-z]+"
			"\\)"
			"\\.html\\)"
			s0 "\"" s0 ">"
			s0 "\\(?:([01]?[0-9]/[0-3]?[0-9])\\)?" s0
			;; 7. subject
			"\\([^<]+\\)"
			"</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5)))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-retto (group folder shimbun range)
  "Function used to fetch headers for the retto group."
  (when (re-search-forward "\
<h[0-9][\t\n ]+class=\"retto-ctgy[^>]+>\\([^\t\n ]+\\)</h[0-9]>"
			   nil t)
    (let ((start (match-end 0))
	  (region (match-string 1))
	  end next subject url serial year month day time headers)
      (while start
	(if (re-search-forward "\
<h[0-9][\t\n ]+class=\"retto-ctgy[^>]+>\\([^\t\n ]+\\)</h[0-9]>"
			       nil t)
	    (setq end (match-end 0)
		  next (match-string 1))
	  (setq end nil))
	(while (progn
		 (goto-char start)
		 (re-search-forward
		  (eval-when-compile
		    (let ((s0 "[\t\n ]*")
			  (s1 "[\t\n ]+"))
		      (concat "<AREA21" s1 "HEADLINE=\""
			      ;; 1. subject
			      "\\([^\"]+\\)"
			      "\"" s1 "URL=\""
			      ;; 2. url
			      "\\("
			      ;; 3. serial number
			      "\\([^\".]+\\)"
			      "\\.html\\)"
			      s0 "\"" s1 "ARTICLE_TIME=\""
			      ;; 4. year
			      "\\(20[0-9][0-9]\\)"
			      "/"
			      ;; 5. month
			      "\\([01][0-9]\\)"
			      "/"
			      ;; 6. day
			      "\\([0-3][0-9]\\)"
			      s1
			      ;; 7. hour:minute
			      "\\([012][0-9]:[0-5][0-9]\\)")))
		  end t))
	  (setq subject (match-string 1)
		url (match-string 2)
		serial (downcase (match-string 3))
		year (string-to-number (match-string 4))
		month (string-to-number (match-string 5))
		day (string-to-number (match-string 6))
		time (match-string 7)
		start (match-end 0))
	  (when (re-search-forward
		 (concat
		  (eval-when-compile
		    (let ((s0 "[\t\n ]*")
			  (s1 "[\t\n ]+"))
		      (concat "<!--" s1 "aLink" s1 "-->" s0 "<a" s1 "HREF=\""
			      ;; 1. url
			      "\\([^\"]+\\)"
			      s0 "\">" s0 "<!--" s1 "headline" s1 "-->" s0)))
		  (regexp-quote subject))
		 end t)
	    (setq url (match-string 1)))
	  (push (shimbun-create-header
		 0
		 (concat "[" region "] " subject)
		 shimbun-nikkei-from-address
		 (shimbun-nikkei-make-date-string year month day time)
		 (concat "<" serial "%" group "."
			 shimbun-nikkei-top-level-domain ">")
		 "" 0 0
		 (shimbun-nikkei-expand-url url folder))
		headers))
	(setq start end
	      region next))
      headers)))

(defun shimbun-nikkei-get-headers-sports (group folder shimbun range)
  "Function used to fetch headers for the sports group."
  ;; Skip headlinenews.
  (re-search-forward "\
<span[\t\n ]+class=\"sub_bar_time\">[\t\n ]*更新：[01]?[0-9]月[0-3]?[0-9]日"
		     nil t)
  (let (category headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(http://sports\\.nikkei\\.co\\.jp/news\\.cfm\\?i="
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[^&]+\\)"
			"&t="
			;; 4. category
			"\\([^\"]+\\)"
			"\\)"
			s0 "\">" s0 "("
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			")" s0
			;; 7. subject
			"\\([^<]+\\)")))
	    nil t)
      (setq category (match-string 4))
      (push (shimbun-create-header
	     0
	     (concat "[" category "] " (match-string 7))
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6)))
	     (concat "<" (downcase (match-string 2)) "%" category "." group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (match-string 1))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-newpro (group folder shimbun range)
  "Function used to fetch headers for the newpro group."
  (when (re-search-forward ">[\t\n ]*最新新製品ニュース[\t\n ]*<" nil t)
    (narrow-to-region (point) (or (search-forward "</ul>" nil t)
				  (point-max)))
    (goto-char (point-min))
    (let (headers)
      (while (re-search-forward
	      (eval-when-compile
		(let ((s0 "[\t\n ]*")
		      (s1 "[\t\n ]+"))
		  (concat "<a" s1 "href=\""
			  ;; 1. url
			  "\\(\\(?:[^\"]+/\\)?"
			  ;; 2. serial number
			  "\\("
			  ;; 3. year
			  "\\(20[0-9][0-9]\\)"
			  ;; 4. month
			  "\\([01][0-9]\\)"
			  ;; 5. day
			  "\\([0-3][0-9]\\)"
			  "[0-9a-z]+\\)"
			  "\\.html\\)"
			  s0 "\"" s0 ">" s0
			  ;; 6. subject
			  "\\([^<]+\\)")))
	      nil t)
	(push (shimbun-create-header
	       0
	       (match-string 6)
	       shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(string-to-number (match-string 3))
		(string-to-number (match-string 4))
		(string-to-number (match-string 5)))
	       (concat "<" (downcase (match-string 2)) "%" group "."
		       shimbun-nikkei-top-level-domain ">")
	       "" 0 0
	       (shimbun-nikkei-expand-url (match-string 1) folder))
	      headers))
      (widen)
      headers)))

(defun shimbun-nikkei-get-headers-release (group folder shimbun range)
  "Function used to fetch headers for the release group."
  (let (url id subject sub-end year month day headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(detail\\.cfm\\?relID="
			;; 2. serial number
			"\\([^\"]+\\)"
			"\\)"
			s0 "\"" s0 ">" s0
			;; 3. subject
			"\\([^<]+\\)")))
	    nil t)
      (setq url (match-string 1)
	    id (downcase (match-string 2))
	    subject (match-string 3)
	    sub-end (point))
      (when (re-search-backward "\
>[\t\n ]*\\(20[0-9][0-9]\\)/\\([01][0-9]\\)/\\([0-3][0-9]\\)[^0-9]"
				nil t)
	(push (shimbun-create-header
	       0
	       subject
	       shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(setq year (string-to-number (match-string 1)))
		(setq month (string-to-number (match-string 2)))
		(setq day (string-to-number (match-string 3))))
	       (format "<%d%02d%02d.%s%%%s.%s>"
		       year month day id group shimbun-nikkei-top-level-domain)
	       "" 0 0
	       (shimbun-nikkei-expand-url url folder))
	      headers)
	(goto-char sub-end)))
    headers))

(defun shimbun-nikkei-get-headers-release2 (group folder shimbun range)
  "Function used to fetch headers for the release-in-detail groups."
  (let ((pages (shimbun-header-index-pages range))
	(count 0)
	url id subject sub-end year month day headers)
    (catch 'stop
      (while t
	(while (re-search-forward
		(eval-when-compile
		  (let ((s0 "[\t\n ]*")
			(s1 "[\t\n ]+"))
		    (concat "<a" s1 "href=\""
			    ;; 1. url
			    "\\(detail\\.cfm\\?relID="
			    ;; 2. serial number
			    "\\([^\"]+\\)"
			    "\\)"
			    s0 "\"" s0 ">" s0
			    ;; 3. subject
			    "\\([^<]+\\)")))
		nil t)
	  (setq url (match-string 1)
		id (downcase (match-string 2))
		subject (match-string 3)
		sub-end (point))
	  (when (re-search-backward "\
>[\t\n ]*\\(20[0-9][0-9]\\)/\\([01][0-9]\\)/\\([0-3][0-9]\\)[^0-9]"
				    nil t)
	    (setq year (string-to-number (match-string 1))
		  month (string-to-number (match-string 2))
		  day (string-to-number (match-string 3))
		  id (format "<%d%02d%02d.%s%%%s.%s>"
			     year month day id group
			     shimbun-nikkei-top-level-domain))
	    (if (shimbun-search-id shimbun id)
		(unless (zerop count)
		  (throw 'stop nil))
	      (push (shimbun-create-header
		     0 subject shimbun-nikkei-from-address
		     (shimbun-nikkei-make-date-string year month day)
		     id "" 0 0
		     (shimbun-nikkei-expand-url
		      (concat "http://release.nikkei.co.jp/" url) folder))
		    headers))
	    (goto-char sub-end)))
	(if (and (or (not pages)
		     (< (setq count (1+ count)) pages))
		 (progn
		   (goto-char (point-min))
		   (re-search-forward "<a[\t\n ]+href=\"\
\\(isclassList\\.cfm\\?page=[0-9]+&lindID=[0-9]+&sindID=[0-9]+\\)\
\[\t\n ]*\"[\t\n ]*>[\t\n ]*次へ[\t\n ]*&gt;[\t\n ]*</a>"
				      nil t)))
	    (progn
	      (shimbun-retrieve-url (prog1
					(concat "http://release.nikkei.co.jp/"
						(match-string 1))
				      (erase-buffer))
				    t)
	      (goto-char (point-min)))
	  (throw 'stop nil))))
    headers))

(defun shimbun-nikkei-get-headers-shasetsu (group folder shimbun range)
  "Function used to fetch headers for the shasetsu group."
  (goto-char (point-max))
  (insert (with-temp-buffer
	    (shimbun-retrieve-url
	     "http://www.nikkei.co.jp/news/shasetsu/IndexKijiIchiran.js"
	     t)
	    (buffer-string)))
  (goto-char (point-min))
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\\\\\""
			;; 1. url
			"\\(/\\(?:[^\"/\\]+/+\\)+"
			;; 2. serial number
			"\\([^\"/\\]+"
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"\\)"
			"\\.html\\)"
			"\\\\\"[^>]*>" s0
			;; 4. subject
			"\\(\\(?:社説\\|春秋\\)[^<]*[(（]" s0
			;; 5. month
			"\\([01]?[0-9]\\)"
			s0 "/" s0
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s0 "[)）]\\)"
			s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (shimbun-subst-char-in-string
	      ?\） ?\)
	      (shimbun-subst-char-in-string ?\（ ?\( (match-string 4)))
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6)))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    (nreverse headers)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-nikkei)
						   header)
  (let ((fn (nth 4 (assoc (shimbun-current-group-internal shimbun)
			  shimbun-nikkei-group-table)))
	(case-fold-search t))
    (while (search-forward "\r" nil t)
      (delete-backward-char 1))
    (goto-char (point-min))
    (while (re-search-forward "[\t\n ]*\\(?:\
<ul\\(?:[\t\n ]+[^>]+\\)?>\
\\|</ul>\
\\|<a[\t\n ]+[^>]+>[\t\n ]*＜拡大\\(?:画像\\)?＞[\t\n ]*</a>\
\\|\\(?:<div[\t\n ]+[^>]+>[\t\n ]*\\)?<img[\t\n ]+src=\"[^\"]+/s\\.gif\"\
\[^>]+>\\(?:[\t\n ]*</div>\\)?\
\\)[\t\n ]*"
			      nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (and (fboundp fn)
	     (funcall fn header))
	(shimbun-with-narrowed-article
	 shimbun
	 ;; Remove <center> tags surrounding images.
	 (while (and (search-forward "<center>" nil t)
		     (progn
		       (backward-char 1)
		       (shimbun-end-of-tag "center" t))
		     (save-match-data
		       (re-search-backward "<img[\t\n ]+"
					   (match-beginning 2) t)))
	   (replace-match "\\3"))
	 ;; Remove useless tags around images.
	 (goto-char (point-min))
	 (while (re-search-forward "[\t\n ]*\
\\(?:<\\(?:\\(?:/?div\\|/?p\\|/?td\\|/?tr\\)\\(?:[\t\n ]*\\|[\t\n ]+[^>]+\\)\
\\|![^>]+\\)>[\t\n ]*\\)+\
\\(<img[\t\n ]+[^>]+>\\)[\t\n ]*\
\\(?:<\\(?:\\(?:/?div\\|/?p\\|/?td\\|/?tr\\)\\(?:[\t\n ]*\\|[\t\n ]+[^>]+\\)\
\\|![^>]+\\)>[\t\n ]*\\)+"
				   nil t)
	   (replace-match "\n\\1<br>\n"))
	 ;; Remove trailing garbage.
	 (goto-char (point-min))
	 (when (re-search-forward "[\t\n ]*\
\\(?:<\\(?:\\(?:/?div\\|/?p\\)\\(?:[\t\n ]*\\|[\t\n ]+[^>]+\\)\\|![^>]+\\)>\
\[\t\n ]*\\)+\\'"
				  nil t)
	   (replace-match "\n"))
	 ;; Break long lines.
	 (shimbun-break-long-japanese-lines))
      (erase-buffer)
      (insert "<html><body>\
Couldn't extract the body for this article.<br>\
Please visit <a href=\""
	      (shimbun-header-xref header)
	      "\"><u>the original page</u></a>.\
</body></html>\n"))
    (goto-char (point-min))))

(defun shimbun-nikkei-prepare-article-default-0 (&rest args)
  "Default function used to prepare contents of an article."
  (let (photo-end body)
    (when (re-search-forward "<table[\t\n ]+id=\"photonews" nil t)
      (delete-region (point-min) (match-beginning 0))
      (when (search-forward "</table>" nil t)
	(setq photo-end (point))))
    (when (or (and (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>[\t\n ]*"
				      nil t)
		   (progn
		     (setq body (point))
		     (re-search-forward "\
\\(?:[\t\n 　]*<\\(?:p\\|p[\t\n 　]+[^>]+\\|/p\\|/p[\t\n 　]+[^>]+\\)>\\)*\
\[\t\n 　]*<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\""
					nil t)))
	      ;; The following section will be used for the `main' group.
	      (and (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"MIDASHI\""
				      nil t)
		   (search-forward "<p>" nil t)
		   (progn
		     (setq body (match-beginning 0))
		     (re-search-forward "<p[^>]\\|\n\n+" nil t))))
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      (if photo-end
	  (progn
	    (delete-region photo-end body)
	    ;; Replace <img src='...'> with <img src="...">.
	    (goto-char (point-min))
	    (while (re-search-forward "<img[\t\n ]+src='\\([^\"']+\\)'"
				      nil t)
	      (replace-match "<img src=\"\\1\""))
	    (goto-char (point-min)))
	(goto-char body))
      (goto-char (point-min))
      (if (and (re-search-forward "<table[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
\\(?:class=\"photo\\|summary=\"写真\\)"
				  nil t)
	       (shimbun-end-of-tag "table" t))
	  (progn
	    (delete-region (match-end 3) body)
	    (insert "\n")
	    (goto-char (match-beginning 3)))
	(goto-char body))
      (insert shimbun-nikkei-content-start)
      t)))

(defun shimbun-nikkei-prepare-article-default (header)
  "Default function used to prepare contents of an article."
  (let (start end)
    (if (or
	 (when (re-search-forward
		"<!-+[\t\n ]*コンテンツ枠[\t\n ]*-+>[\t\n ]*"
		nil t)
	   (setq start (match-end 0))
	   (or (re-search-forward "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
<!-+[\t\n ]*//コンテンツ枠[\t\n ]*-+>[\t\n ]*"
				  nil t)
	       (prog1 nil (goto-char (point-min)))))
	 (when (re-search-forward
		"<!-+[\t\n ]*編成コンテンツ枠[\t\n ]*-+>[\t\n ]*"
		nil t)
	   (setq start (match-end 0))
	   (or (re-search-forward "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
<!-+[\t\n ]*//編成コンテンツ枠[\t\n ]*-+>[\t\n ]*"
				  nil t)
	       (prog1 nil (goto-char (point-min)))))
	 (when (re-search-forward "<!--photo-->[\t\n ]*" nil t)
	   (setq start (match-end 0))
	   (or (re-search-forward "\
\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\\[[01]?[0-9]月[0-3]?[0-9]日[/／][^]]+\\]"
				  nil t)
	       (prog1 nil (goto-char (point-min)))))
	 (when (re-search-forward "\
\\(<!-+[\t\n ]*Photo[\t\n _]+news[\t\n ]*-+>[\t\n ]*\\)\
\\|<\\([^\t\n >]+\\)[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
\\(?:class=\"photo\"\\|summary=\"写真ニュース\"\\)[^>]*>[\t\n ]*\
\\|<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME[\t\n ]*\
=[\t\n ]*\"[\t\n ]*HONBUN[\t\n ]*\"[\t\n ]*-+>[\t\n ]*"
				  nil t)
	   (let (st nd)
	     (if (and (or (and (match-beginning 1)
			       (progn
				 (setq st (match-end 1))
				 (re-search-forward "\
\[\t\n ]*<!-+[\t\n ]*/Photo[\t\n _]news[\t\n ]*-+>"
						    nil t)
				 (setq nd (match-beginning 0))))
			  (and (match-beginning 2)
			       (shimbun-end-of-tag (match-string 2) t)
			       (setq st (match-beginning 3)
				     nd (match-end 3))))
		      (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME[\t\n ]*=[\t\n ]*\"HONBUN\"[\t\n ]*\
-+>[\t\n ]*"
					 nil t))
		 (progn
		   (setq start st)
		   (delete-region nd (match-end 0))
		   (insert "\n"))
	       (setq start (match-end 0))))
	   (or (re-search-forward "\\(?:[\t\n ]*<[^/][^>]*>\\)*[\t\n ]*\
<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME[\t\n ]*\
=[\t\n ]*\"[\t\n ]*HONBUN[\t\n ]*\"[\t\n ]*-+>"
				  nil t)
	       (prog1 nil (goto-char (point-min)))))
	 (when (or (re-search-forward
		    "<!-+[\t\n ]*特集記事大[\t\n ]*-+>[\t\n ]*"
		    nil t)
		   (re-search-forward "<!-+[\t\n ]*記事[\t\n ]*-+>[\t\n ]*"
				      nil t))
	   (setq start (match-end 0))
	   (or (re-search-forward "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
<!-+[\t\n ]*\\(?://記事\\|特集記事フッタ\\)[\t\n ]*-+>"
				  nil t)
	       (prog1 nil (goto-char (point-min)))))
	 (setq end (shimbun-nikkei-prepare-article-default-0 header))
	 (prog1 nil (goto-char (point-min)))

	 ;; Filters having a potential to mistakenly extract the body follow.
	 (when (or (re-search-forward "\
<a[\t\n ]+href=\"\\./\">[\t\n ]*トップ[\t\n ]*</a>[\t\n ]*"
				      nil t)
		   (re-search-forward "\
<div[\t\n ]+class=\"title[^\"]*\">\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*"
				      nil t)
		   (re-search-forward "\
<a[\t\n ]+href=\"\\./\">\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*</a>\
\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*"
				      nil t))
	   (setq start (match-end 0))
	   (or (re-search-forward "\
\[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*<p\\(?:[\t\n ]+[^>]+\\)*[\t\n ]align="
				  nil t)
	       (prog1 nil (goto-char (point-min)))))
	 (when (or (re-search-forward "\
<!-+[\t\n ]*写真[\t\n ]*-+>\\(?:[\t\n ]*<[^i][^>]*>\\)*[\t\n ]*"
				      nil t)
		   (re-search-forward "<!-+[\t\n ]*本文[\t\n ]*-+>" nil t)
		   (re-search-forward "<div[\t\n ]+class=[^>]+>[\t\n ]*"
				      nil t))
	   (setq start (match-end 0))
	   (set-match-data nil)
	   (while (re-search-forward "[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*\
<!-+[\t\n ]*本文[\t\n ]*-+>"
				     nil t))
	   (or (match-beginning 0)
	       (prog1 nil (goto-char (point-min))))))
	(progn
	  (unless end
	    (goto-char (match-beginning 0))
	    (insert shimbun-nikkei-content-end)
	    (goto-char start)
	    (insert shimbun-nikkei-content-start))
	  t)
      (when (and (re-search-forward "\
<a[\t\n ]+[^>]+>[\t\n ]*＞＞記事を読む[\t\n ]*</a>"
				    nil t)
		 (re-search-backward "href=\"\\([^\"]+\\)"
				     (match-beginning 0) t))
	(let ((new (match-string 1))
	      (old (shimbun-header-xref header)))
	  (when (string-match "[^/]/[^/]" old)
	    (setq new (shimbun-nikkei-expand-url
		       new (substring old 0 (1- (match-end 0)))))
	    (shimbun-header-set-xref header new)
	    (erase-buffer)
	    (shimbun-retrieve-url new t)
	    (goto-char (point-min))
	    (shimbun-nikkei-prepare-article-default header)))))))

(defun shimbun-nikkei-prepare-article-default2 (&rest args)
  "Function used to prepare contents of an article for some groups."
  ;; Remove unwanted images.
  (let (start end)
    (while (re-search-forward "[\t\n ]*<div[\t\n ]+[^>]+>[\t\n ]*<img[\t\n ]+\
\[^>]+>[\t\n ]*</div>[\t\n ]*"
			      nil t)
      (setq start (match-beginning 0)
	    end (match-end 0))
      (goto-char start)
      (if (re-search-forward
	   "src=\"http://parts\\.nikkei\\.co\\.jp/parts/s\\.gif\""
	   end t)
	  (delete-region start end)
	(goto-char end))))
  (goto-char (point-min))
  (when (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
\\(?:[\t\n 　]*<\\(?:p\\|p[\t\n 　]+[^>]+\\|/p\\|/p[\t\n 　]+[^>]+\\)>\\)*\
\[\t\n 　]*<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>\
\\|<a[\t\n ]+name=\"newslist\"></a>\n"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      t)))

(defun shimbun-nikkei-prepare-article-kansai (header)
  "Function used to prepare contents of an article for the kansai group."
  (let ((date (when (re-search-forward "[\t\n ]*\
\\(20[0-9][0-9]\\)/\\([01]?[0-9]\\)/\\([0-3][0-9]\\)[\t\n ]*配信[\t\n ]*<"
				       nil t)
		(prog1
		    (shimbun-nikkei-make-date-string
		     (string-to-number (match-string 1))
		     (string-to-number (match-string 2))
		     (string-to-number (match-string 3)))
		  (goto-char (point-min))))))
    (when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"detailframe\\(?: clearfix\"\\)?"
				  nil t)
	       (shimbun-end-of-tag "div" t))
      (goto-char (match-end 3))
      (insert "\n" shimbun-nikkei-content-end)
      (goto-char (match-beginning 3))
      (insert shimbun-nikkei-content-start)
      (when date
	(shimbun-header-set-date header date))
      t)))

(defun shimbun-nikkei-prepare-article-sports (&rest args)
  "Function used to prepare contents of an article for the sports group."
  (when (re-search-forward "\
<\\([^\t\n >]+\\)[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
\\(?:class=\"photo\"\\|summary=\"写真ニュース\"\\)[^>]*>[\t\n ]*\
\\|<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"MIDASHI\"[\t\n ]*-+>[\t\n ]*"
			   nil t)
    (let ((start (match-end 0))
	  st nd)
      (if (and (match-beginning 1)
	       (progn
		 (goto-char (match-beginning 1))
		 (shimbun-end-of-tag (match-string 1) t))
	       (progn
		 (setq st (match-beginning 3)
		       nd (match-end 3))
		 (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME[\t\n ]*=[\t\n ]*\"HONBUN\"[\t\n ]*\
-+>[\t\n ]*"
				    nil t)))
	  (progn
	    (delete-region nd (match-end 0))
	    (insert "\n")
	    (goto-char st))
	(goto-char start)))
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      t)))

(defun shimbun-nikkei-prepare-article-newpro (&rest args)
  "Function used to prepare contents of an article for the newpro group."
  (let (photo-end body)
    (when (re-search-forward "<table[\t\n ]+id=\"photonews" nil t)
      (delete-region (point-min) (match-beginning 0))
      (when (search-forward "</table>" nil t)
	(setq photo-end (point))))
    (when (and (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>[\t\n ]*"
				  nil t)
	       (setq body (point))
	       (re-search-forward "\
\[\t\n ]*<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\""
				  nil t))
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      (if photo-end
	  (progn
	    (delete-region photo-end body)
	    (goto-char (point-min)))
	(goto-char body))
      (insert shimbun-nikkei-content-start)
      t)))

(defun shimbun-nikkei-prepare-article-release (&rest args)
  "Function used to prepare contents of an article for the release group."
  (shimbun-remove-tags "<p[\t\n ]+class=\"re_print\"" "</p>")
  (goto-char (point-min))
  (when (re-search-forward "<[\t\n ]*TD[\t\n ]+colspan=\"3\">" nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "[\t\n ]*<div[\t\n ]+class=\"tokushu\">" nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      t)))

(defun shimbun-nikkei-prepare-article-release2 (&rest args)
  "Function used to prepare contents of an article for the release groups."
  (when (re-search-forward ">[\t\n ]*このページをプリントする[\t\n ]*\
\\(?:\\(?:<[^>]+>*[\t\n ]*\\)*<h[0-9]+\\(?:[\t\n ]+[^>]+\\)*\
\[\t\n ]+[^\t\n >]+[\t\n ]*=[\t\n ]*\"[\t\n ]*heading[\t\n ]*\"[^>]*>\
\[^<]+</h[0-9]+>\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\\)?"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (and (re-search-forward "[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*\
<div\\(?:[\t\n ]+[^>]+\\)*[\t\n ]+class[\t\n ]*=[\t\n ]*\"[\t\n ]*\
tokushu[\t\n ]*\""
			    nil t)
	 (goto-char (match-beginning 0)))
    (insert shimbun-nikkei-content-end)
    t))

(defun shimbun-nikkei-prepare-article-market (header)
  "Function used to prepare contents of an article for the market group."
  (when (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\\(?:(\\([012]?[0-9]:[0-5]?[0-9]\\))[\t\n ]*\\)?\
<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>"
			     nil t)
      (if (match-beginning 1)
	  (progn
	    (goto-char (1+ (match-end 1)))
	    (let ((new (match-string 1))
		  (date (shimbun-header-date header)))
	      (when (string-match "[012]?[0-9]:[0-5]?[0-9]" date)
		(shimbun-header-set-date
		 header (replace-match new nil nil date)))))
	(goto-char (match-beginning 0)))
      (insert shimbun-nikkei-content-end)
      t)))

(defun shimbun-nikkei-prepare-article-default4 (&rest args)
  "Function used to prepare contents of an article for some groups."
  (when (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>\
\\|</h2>\n[\t\n ]+<p>"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>\
\\|<script[\t\n ]+language=\"javascript\"[\t\n ]+type=\"text/javascript\">"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      t)))

(provide 'sb-nikkei)

;;; sb-nikkei.el ends here
