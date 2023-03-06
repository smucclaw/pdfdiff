

all: html org matrices

matrixFiles := FTA/1._Goods/matrix-TradeInGoods.org \
               FTA/1._Goods/matrix-RulesOfOrigin.org \
               FTA/2._Services/matrix-TradeInServices.org \
               FTA/3._Investment/matrix-Investment.org

clean:
	rm FTA/*/*/*.md FTA/*/*/*.org FTA/*/*/*.html FTA/*/matrices-*.org

%.docx : %.pdf
	@echo "* [ERROR] Please use Adobe Acrobat to export" $< "to Docx. This is a mostly manual step."
	@if [ -e $@ ]; then touch $@; fi

%.md : %.docx
	pandoc $^ --wrap=none --lua-filter=lua/blockquote_to_plain.lua -t markdown -o $@

html:
	@echo "* converting docx to HTML"
	parallel pandoc {} --wrap=none --lua-filter=lua/blockquote_to_plain.lua -t html     -o {.}.html ::: FTA/*/*/*.docx

org:
	@echo "* converting docx to org"
	parallel pandoc {} --wrap=none --lua-filter=lua/blockquote_to_plain.lua -t org      -o {.}.org  ::: FTA/*/*/*.docx

cleanmatrices:
	rm $(matrixFiles)

matrices: $(matrixFiles)

FTA/1._Goods/matrix-TradeInGoods.org: FTA/1._Goods/SAFTA/Chapter_2_-Trade_in_Goods.md    \
                                      FTA/1._Goods/ANZSCEP/Chapter_2_-_Trade_in_Goods.md \
                                      FTA/1._Goods/CSFTA/Chapter_3_-_Trade_in_Goods.md
	@echo "* building $@"
	stack run -- $^ > $@

FTA/1._Goods/matrix-RulesOfOrigin.org: FTA/1._Goods/SAFTA/Chapter_3_-_Rules_of_Origin.md      \
                                       FTA/1._Goods/ANZSCEP/Chapter_3_-_Rules_of_Origin_and_Origin_Procedures.md \
                                       FTA/1._Goods/CSFTA/Chapter_4_-_Rules_of_Origin.md
	@echo "* building $@"
	stack run -- $^ > $@

FTA/2._Services/matrix-TradeInServices.org: FTA/2._Services/ANZSCEP/Chapter_8_-_Services.md        \
                                            FTA/2._Services/SAFTA/Chapter_7_-_Trade_in_Services.md \
                                            FTA/2._Services/CSFTA/Chapter_8_-_Trade_in_Services.md
	@echo "* building $@"
	stack run -- $^ > $@

FTA/3._Investment/matrix-Investment.org: FTA/3._Investment/ANZSCEP/Chapter_7_-_Investment.md \
                                         FTA/3._Investment/SAFTA/Chapter_8_-_Investment.md   \
                                         FTA/3._Investment/CSFTA/Chapter_10_-_Investment.md
	@echo "* building $@"
	stack run -- $< > $@
