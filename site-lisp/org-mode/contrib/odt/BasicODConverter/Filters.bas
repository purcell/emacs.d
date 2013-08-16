REM  *****  BASIC  *****

Dim DocTypes

Private DocTypeToFiltersMap As New Collection
Private WriterExportFilters As New Collection
Private WriterWebExportFilters As New Collection
Private CalcExportFilters As New Collection
Private ImpressExportFilters As New Collection
Private DrawExportFilters As New Collection


Private ExportFiltersInited As Boolean

Sub InitExportFilters
	If ExportFiltersInited Then
		Exit Sub
	End If

 	DocTypes = Array(_
			 "com.sun.star.text.TextDocument", _
			 "com.sun.star.sheet.SpreadsheetDocument", _
			 "com.sun.star.presentation.PresentationDocument", _
			 "com.sun.star.drawing.DrawingDocument",_
			 "com.sun.star.text.WebDocument"_
			 )
	With WriterExportFilters
		.Add Key := "bib"       , Item :=Array("bib"   , "BibTeX"                                                       , "BibTeX_Writer                                 ")
		.Add Key := "doc"       , Item :=Array("doc"   , "Microsoft Word 97/2000/XP"                                    , "MS Word 97                                    ")
		.Add Key := "doc6"      , Item :=Array("doc"   , "Microsoft Word 6.0"                                           , "MS WinWord 6.0                                ")
		.Add Key := "doc95"     , Item :=Array("doc"   , "Microsoft Word 95"                                            , "MS Word 95                                    ")
		.Add Key := "docbook"   , Item :=Array("xml"   , "DocBook"                                                      , "DocBook File                                  ")
		.Add Key := "html"      , Item :=Array("html"  , "HTML Document (OpenOffice.org Writer)"                        , "HTML (StarWriter)                             ")
		.Add Key := "latex"     , Item :=Array("ltx"   , "LaTeX 2e"                                                     , "LaTeX_Writer                                  ")
		.Add Key := "mediawiki" , Item :=Array("txt"   , "MediaWiki"                                                    , "MediaWiki                                     ")
		.Add Key := "odt"       , Item :=Array("odt"   , "ODF Text Document"                                            , "writer8                                       ")
		.Add Key := "ooxml"     , Item :=Array("xml"   , "Microsoft Office Open XML"                                    , "MS Word 2003 XML                              ")
		.Add Key := "ott"       , Item :=Array("ott"   , "Open Document Text"                                           , "writer8_template                              ")
		.Add Key := "pdf"       , Item :=Array("pdf"   , "Portable Document Format"                                     , "writer_pdf_Export                             ")
		.Add Key := "rtf"       , Item :=Array("rtf"   , "Rich Text Format"                                             , "Rich Text Format                              ")
		.Add Key := "sdw"       , Item :=Array("sdw"   , "StarWriter 5.0"                                               , "StarWriter 5.0                                ")
		.Add Key := "sdw3"      , Item :=Array("sdw"   , "StarWriter 3.0"                                               , "StarWriter 3.0                                ")
		.Add Key := "sdw4"      , Item :=Array("sdw"   , "StarWriter 4.0"                                               , "StarWriter 4.0                                ")
		.Add Key := "stw"       , Item :=Array("stw"   , "Open Office.org 1.0 Text Document Template"                   , "writer_StarOffice_XML_Writer_Template         ")
		.Add Key := "sxw"       , Item :=Array("sxw"   , "Open Office.org 1.0 Text Document"                            , "StarOffice XML (Writer)                       ")
		.Add Key := "text"      , Item :=Array("txt"   , "Text Encoded"                                                 , "Text (encoded)                                ")
		.Add Key := "txt"       , Item :=Array("txt"   , "Text"                                                         , "Text                                          ")
		.Add Key := "uot"       , Item :=Array("uot"   , "Unified Office Format text"                                   , "UOF text                                      ")
		.Add Key := "vor"       , Item :=Array("vor"   , "StarWriter 5.0 Template"                                      , "StarWriter 5.0 Vorlage/Template               ")
		.Add Key := "vor3"      , Item :=Array("vor"   , "StarWriter 3.0 Template"                                      , "StarWriter 3.0 Vorlage/Template               ")
		.Add Key := "vor4"      , Item :=Array("vor"   , "StarWriter 4.0 Template"                                      , "StarWriter 4.0 Vorlage/Template               ")
		.Add Key := "xhtml"     , Item :=Array("html"  , "XHTML Document"                                               , "XHTML Writer File                             ")
	End With

	With DrawExportFilters
		.Add Key := "bmp"       , Item :=Array("bmp"   , "Windows Bitmap"                                               , "draw_bmp_Export                               ")
		.Add Key := "emf"       , Item :=Array("emf"   , "Enhanced Metafile"                                            , "draw_emf_Export                               ")
		.Add Key := "eps"       , Item :=Array("eps"   , "Encapsulated PostScript"                                      , "draw_eps_Export                               ")
		.Add Key := "gif"       , Item :=Array("gif"   , "Graphics Interchange Format"                                  , "draw_gif_Export                               ")
		.Add Key := "html"      , Item :=Array("html"  , "HTML Document (OpenOffice.org Draw)"                          , "draw_html_Export                              ")
		.Add Key := "jpg"       , Item :=Array("jpg"   , "Joint Photographic Experts Group"                             , "draw_jpg_Export                               ")
		.Add Key := "met"       , Item :=Array("met"   , "OS/2 Metafile"                                                , "draw_met_Export                               ")
		.Add Key := "odd"       , Item :=Array("odd"   , "OpenDocument Drawing"                                         , "draw8                                         ")
		.Add Key := "otg"       , Item :=Array("otg"   , "OpenDocument Drawing Template"                                , "draw8_template                                ")
		.Add Key := "pbm"       , Item :=Array("pbm"   , "Portable Bitmap"                                              , "draw_pbm_Export                               ")
		.Add Key := "pct"       , Item :=Array("pct"   , "Mac Pict"                                                     , "draw_pct_Export                               ")
		.Add Key := "pdf"       , Item :=Array("pdf"   , "Portable Document Format"                                     , "draw_pdf_Export                               ")
		.Add Key := "pgm"       , Item :=Array("pgm"   , "Portable Graymap"                                             , "draw_pgm_Export                               ")
		.Add Key := "png"       , Item :=Array("png"   , "Portable Network Graphic"                                     , "draw_png_Export                               ")
		.Add Key := "ppm"       , Item :=Array("ppm"   , "Portable Pixelmap"                                            , "draw_ppm_Export                               ")
		.Add Key := "ras"       , Item :=Array("ras"   , "Sun Raster Image"                                             , "draw_ras_Export                               ")
		.Add Key := "std"       , Item :=Array("std"   , "OpenOffice.org 1.0 Drawing Template"                          , "draw_StarOffice_XML_Draw_Template             ")
		.Add Key := "svg"       , Item :=Array("svg"   , "Scalable Vector Graphics"                                     , "draw_svg_Export                               ")
		.Add Key := "svm"       , Item :=Array("svm"   , "StarView Metafile"                                            , "draw_svm_Export                               ")
		.Add Key := "swf"       , Item :=Array("swf"   , "Macromedia Flash (SWF)"                                       , "draw_flash_Export                             ")
		.Add Key := "sxd"       , Item :=Array("sxd"   , "OpenOffice.org 1.0 Drawing"                                   , "StarOffice XML (Draw)                         ")
		.Add Key := "sxd3"      , Item :=Array("sxd"   , "StarDraw 3.0"                                                 , "StarDraw 3.0                                  ")
		.Add Key := "sxd5"      , Item :=Array("sxd"   , "StarDraw 5.0"                                                 , "StarDraw 5.0                                  ")
		.Add Key := "tiff"      , Item :=Array("tiff"  , "Tagged Image File Format"                                     , "draw_tif_Export                               ")
		.Add Key := "vor"       , Item :=Array("vor"   , "StarDraw 5.0 Template"                                        , "StarDraw 5.0 Vorlage                          ")
		.Add Key := "vor3"      , Item :=Array("vor"   , "StarDraw 3.0 Template"                                        , "StarDraw 3.0 Vorlage                          ")
		.Add Key := "wmf"       , Item :=Array("wmf"   , "Windows Metafile"                                             , "draw_wmf_Export                               ")
		.Add Key := "xhtml"     , Item :=Array("xhtml" , "XHTML"                                                        , "XHTML Draw File                               ")
		.Add Key := "xpm"       , Item :=Array("xpm"   , "X PixMap"                                                     , "draw_xpm_Export                               ")


	End With

	With ImpressExportFilters
		.Add Key := "bmp"       , Item :=Array("bmp"   , "Windows Bitmap"                                               , "impress_bmp_Export                            ")
		.Add Key := "emf"       , Item :=Array("emf"   , "Enhanced Metafile"                                            , "impress_emf_Export                            ")
		.Add Key := "eps"       , Item :=Array("eps"   , "Encapsulated PostScript"                                      , "impress_eps_Export                            ")
		.Add Key := "gif"       , Item :=Array("gif"   , "Graphics Interchange Format"                                  , "impress_gif_Export                            ")
		.Add Key := "html"      , Item :=Array("html"  , "HTML Document (OpenOffice.org Impress)"                       , "impress_html_Export                           ")
		.Add Key := "jpg"       , Item :=Array("jpg"   , "Joint Photographic Experts Group"                             , "impress_jpg_Export                            ")
		.Add Key := "met"       , Item :=Array("met"   , "OS/2 Metafile"                                                , "impress_met_Export                            ")
		.Add Key := "odg"       , Item :=Array("odg"   , "ODF Drawing (Impress)"                                        , "impress8_draw                                 ")
		.Add Key := "odp"       , Item :=Array("odp"   , "ODF Presentation"                                             , "impress8                                      ")
		.Add Key := "otp"       , Item :=Array("otp"   , "ODF Presentation Template"                                    , "impress8_template                             ")
		.Add Key := "pbm"       , Item :=Array("pbm"   , "Portable Bitmap"                                              , "impress_pbm_Export                            ")
		.Add Key := "pct"       , Item :=Array("pct"   , "Mac Pict"                                                     , "impress_pct_Export                            ")
		.Add Key := "pdf"       , Item :=Array("pdf"   , "Portable Document Format"                                     , "impress_pdf_Export                            ")
		.Add Key := "pgm"       , Item :=Array("pgm"   , "Portable Graymap"                                             , "impress_pgm_Export                            ")
		.Add Key := "png"       , Item :=Array("png"   , "Portable Network Graphic"                                     , "impress_png_Export                            ")
		.Add Key := "pot"       , Item :=Array("pot"   , "Microsoft PowerPoint 97/2000/XP Template"                     , "MS PowerPoint 97 Vorlage                      ")
		.Add Key := "ppm"       , Item :=Array("ppm"   , "Portable Pixelmap"                                            , "impress_ppm_Export                            ")
		.Add Key := "ppt"       , Item :=Array("ppt"   , "Microsoft PowerPoint 97/2000/XP"                              , "MS PowerPoint 97                              ")
		.Add Key := "pwp"       , Item :=Array("pwp"   , "PlaceWare"                                                    , "placeware_Export                              ")
		.Add Key := "ras"       , Item :=Array("ras"   , "Sun Raster Image"                                             , "impress_ras_Export                            ")
		.Add Key := "sda"       , Item :=Array("sda"   , "StarDraw 5.0 (OpenOffice.org Impress)"                        , "StarDraw 5.0 (StarImpress)                    ")
		.Add Key := "sdd"       , Item :=Array("sdd"   , "StarImpress 5.0"                                              , "StarImpress 5.0                               ")
		.Add Key := "sdd3"      , Item :=Array("sdd"   , "StarDraw 3.0 (OpenOffice.org Impress)"                        , "StarDraw 3.0 (StarImpress)                    ")
		.Add Key := "sdd4"      , Item :=Array("sdd"   , "StarImpress 4.0"                                              , "StarImpress 4.0                               ")
		.Add Key := "sti"       , Item :=Array("sti"   , "OpenOffice.org 1.0 Presentation Template"                     , "impress_StarOffice_XML_Impress_Template       ")
		.Add Key := "svg"       , Item :=Array("svg"   , "Scalable Vector Graphics"                                     , "impress_svg_Export                            ")
		.Add Key := "svm"       , Item :=Array("svm"   , "StarView Metafile"                                            , "impress_svm_Export                            ")
		.Add Key := "swf"       , Item :=Array("swf"   , "Macromedia Flash (SWF)"                                       , "impress_flash_Export                          ")
		.Add Key := "sxd"       , Item :=Array("sxd"   , "OpenOffice.org 1.0 Drawing (OpenOffice.org Impress)"          , "impress_StarOffice_XML_Draw                   ")
		.Add Key := "sxi"       , Item :=Array("sxi"   , "OpenOffice.org 1.0 Presentation"                              , "StarOffice XML (Impress)                      ")
		.Add Key := "tiff"      , Item :=Array("tiff"  , "Tagged Image File Format"                                     , "impress_tif_Export                            ")
		.Add Key := "uop"       , Item :=Array("uop"   , "Unified Office Format presentation"                           , "UOF presentation                              ")
		.Add Key := "vor"       , Item :=Array("vor"   , "StarImpress 5.0 Template"                                     , "StarImpress 5.0 Vorlage                       ")
		.Add Key := "vor3"      , Item :=Array("vor"   , "StarDraw 3.0 Template (OpenOffice.org Impress)"               , "StarDraw 3.0 Vorlage (StarImpress)            ")
		.Add Key := "vor4"      , Item :=Array("vor"   , "StarImpress 4.0 Template"                                     , "StarImpress 4.0 Vorlage                       ")
		.Add Key := "vor5"      , Item :=Array("vor"   , "StarDraw 5.0 Template (OpenOffice.org Impress)"               , "StarDraw 5.0 Vorlage (StarImpress)            ")
		.Add Key := "wmf"       , Item :=Array("wmf"   , "Windows Metafile"                                             , "impress_wmf_Export                            ")
		.Add Key := "xhtml"     , Item :=Array("xml"   , "XHTML"                                                        , "XHTML Impress File                            ")
		.Add Key := "xpm"       , Item :=Array("xpm"   , "X PixMap"                                                     , "impress_xpm_Export                            ")

	End With

	With CalcExportFilters
		.Add Key := "csv"       , Item :=Array("csv"   , "Text CSV"                                                     , "Text - txt - csv (StarCalc)                   ")
		.Add Key := "dbf"       , Item :=Array("dbf"   , "dBASE"                                                        , "dBase                                         ")
		.Add Key := "dif"       , Item :=Array("dif"   , "Data Interchange Format"                                      , "DIF                                           ")
		.Add Key := "html"      , Item :=Array("html"  , "HTML Document (OpenOffice.org Calc)"                          , "HTML (StarCalc)                               ")
		.Add Key := "ods"       , Item :=Array("ods"   , "ODF Spreadsheet"                                              , "calc8                                         ")
		.Add Key := "ooxml"     , Item :=Array("xml"   , "Microsoft Excel 2003 XML"                                     , "MS Excel 2003 XML                             ")
		.Add Key := "ots"       , Item :=Array("ots"   , "ODF Spreadsheet Template"                                     , "calc8_template                                ")
		.Add Key := "pdf"       , Item :=Array("pdf"   , "Portable Document Format"                                     , "calc_pdf_Export                               ")
		.Add Key := "sdc"       , Item :=Array("sdc"   , "StarCalc 5.0"                                                 , "StarCalc 5.0                                  ")
		.Add Key := "sdc3"      , Item :=Array("sdc"   , "StarCalc 3.0"                                                 , "StarCalc 3.0                                  ")
		.Add Key := "sdc4"      , Item :=Array("sdc"   , "StarCalc 4.0"                                                 , "StarCalc 4.0                                  ")
		.Add Key := "slk"       , Item :=Array("slk"   , "SYLK"                                                         , "SYLK                                          ")
		.Add Key := "stc"       , Item :=Array("stc"   , "OpenOffice.org 1.0 Spreadsheet Template"                      , "calc_StarOffice_XML_Calc_Template             ")
		.Add Key := "sxc"       , Item :=Array("sxc"   , "OpenOffice.org 1.0 Spreadsheet"                               , "StarOffice XML (Calc)                         ")
		.Add Key := "uos"       , Item :=Array("uos"   , "Unified Office Format spreadsheet"                            , "UOF spreadsheet                               ")
		.Add Key := "vor"       , Item :=Array("vor"   , "StarCalc 5.0 Template"                                        , "StarCalc 5.0 Vorlage/Template                 ")
		.Add Key := "vor3"      , Item :=Array("vor"   , "StarCalc 3.0 Template"                                        , "StarCalc 3.0 Vorlage/Template                 ")
		.Add Key := "vor4"      , Item :=Array("vor"   , "StarCalc 4.0 Template"                                        , "StarCalc 4.0 Vorlage/Template                 ")
		.Add Key := "xhtml"     , Item :=Array("xhtml" , "XHTML"                                                        , "XHTML Calc File                               ")
		.Add Key := "xls"       , Item :=Array("xls"   , "Microsoft Excel 97/2000/XP"                                   , "MS Excel 97                                   ")
		.Add Key := "xls5"      , Item :=Array("xls"   , "Microsoft Excel 5.0"                                          , "MS Excel 5.0/95                               ")
		.Add Key := "xls95"     , Item :=Array("xls"   , "Microsoft Excel 95"                                           , "MS Excel 95                                   ")
		.Add Key := "xlt"       , Item :=Array("xlt"   , "Microsoft Excel 97/2000/XP Template"                          , "MS Excel 97 Vorlage/Template                  ")
		.Add Key := "xlt5"      , Item :=Array("xlt"   , "Microsoft Excel 5.0 Template"                                 , "MS Excel 5.0/95 Vorlage/Template              ")
		.Add Key := "xlt95"     , Item :=Array("xlt"   , "Microsoft Excel 95 Template"                                  , "MS Excel 95 Vorlage/Template                  ")

	End With

	With WriterWebExportFilters
		.Add Key := "etext"     , Item :=Array("txt"   , "Text Encoded (OpenOffice.org Writer/Web)"                     , "Text (encoded) (StarWriter/Web)               ")
		.Add Key := "html"      , Item :=Array("html"  , "HTML Document"                                                , "HTML                                          ")
		'.Add Key := "html"      , Item :=Array("html"  , "HTML Document Template"                                       , "writerweb8_writer_template                    ")
		.Add Key := "html10"    , Item :=Array("html"  , "OpenOffice.org 1.0 HTML Template"                             , "writer_web_StarOffice_XML_Writer_Web_Template ")
		.Add Key := "mediawiki" , Item :=Array("txt"   , "MediaWiki"                                                    , "MediaWiki_Web                                 ")
		.Add Key := "pdf"       , Item :=Array("pdf"   , "PDF - Portable Document Format"                               , "writer_web_pdf_Export                         ")
		.Add Key := "sdw"       , Item :=Array("sdw"   , "StarWriter 5.0 (OpenOffice.org Writer/Web)"                   , "StarWriter 5.0 (StarWriter/Web)               ")
		.Add Key := "sdw3"      , Item :=Array("sdw"   , "StarWriter 3.0 (OpenOffice.org Writer/Web)"                   , "StarWriter 3.0 (StarWriter/Web)               ")
		.Add Key := "sdw4"      , Item :=Array("sdw"   , "StarWriter 4.0 (OpenOffice.org Writer/Web)"                   , "StarWriter 4.0 (StarWriter/Web)               ")
		.Add Key := "text"      , Item :=Array("txt"   , "Text (OpenOffice.org Writer/Web)"                             , "Text (StarWriter/Web)                         ")
		.Add Key := "text10"    , Item :=Array("txt"   , "OpenOffice.org 1.0 Text Document (OpenOffice.org Writer/Web)" , "writer_web_StarOffice_XML_Writer              ")
		.Add Key := "odt"       , Item :=Array("txt"   , "OpenOffice.org Text (OpenOffice.org Writer/Web)"              , "writerweb8_writer                             ")
		.Add Key := "vor"       , Item :=Array("vor"   , "StarWriter/Web 5.0 Template"                                  , "StarWriter/Web 5.0 Vorlage/Template           ")
		.Add Key := "vor4"      , Item :=Array("vor"   , "StarWriter/Web 4.0 Template"                                  , "StarWriter/Web 4.0 Vorlage/Template           ")

	End With

	With DocTypeToFiltersMap
		.Add Key := "com.sun.star.text.TextDocument", Item := WriterExportFilters
		.Add Key := "com.sun.star.sheet.SpreadsheetDocument", Item := CalcExportFilters
		.Add Key := "com.sun.star.presentation.PresentationDocument", Item :=ImpressExportFilters
		.Add Key := "com.sun.star.drawing.DrawingDocument", Item := DrawExportFilters
		.Add Key := "com.sun.star.text.WebDocument", Item := WriterWebExportFilters
	End With
	ExportFiltersInited = True
End Sub

Function FilterSaveExtension(filterDescriptor ())
	FilterSaveExtension = Trim(filterDescriptor(0))
End Function

Function FilterHandler(filterDescriptor ())
	FilterHandler = Trim(filterDescriptor(2))
End Function

Function GetFilter(docType, outputFormat)
	Dim filters

	On Error Goto MissingFilter
	filters = DocTypeToFiltersMap(docType)
	LogMessage "output format is " & outputFormat
	GetFilter = filters(outputFormat)

Done:
	Exit Function

MissingFilter:
	LogMessage("No existing filters for exporting " & docType & " to " & outputFormat)
	GetFilter = Null
	Resume Done
End Function

