﻿document.writeln("<div id=\"loading\" style=\"display:none\"><table width=\"100%\" cellspacing=\"5\"><tr><td>");
document.writeln("<textarea name=\"{textareaco}\" id=\"{textareaco}\">使用帮助：在右侧选择一个处理模式后再按下面的“开始处理”按钮。");
document.writeln("使用注意：1.请注意是否需要将标题复制进来处理 2.请不要将网址一同复制进来过滤，即使你看不出网址有被修改 3.暂不支持繁体字识别！ 4.网站有可能识别出某些敏感词的谐音字和英文 5.很多网站会识别出由标点、英文和数字隔开的敏感词，如“一到做操，死人一样站着不动”本工具暂不能检测出此句中的敏感词“操死”！");
document.writeln("法律声明：仅供于因脑残的网站敏感词检测系统导致正常帖发不出时使用，不得用于发布非法内容！");
document.writeln("");
document.writeln("敏感词过滤测试：");
document.writeln("·伟大的祖国母亲我爱你！");
document.writeln("·一次性交纳五百元送话费！");
document.writeln("·出售二手24口交换机及一台独立服务器，要的手机联系我……");
document.writeln("·宝鸡巴士公司真诚欢迎来自江阴毛纺厂和江阴道路管理局及上海虹口交警支队的保持党员先进性爱国主义教育小组的领导们！下午将参观模拟开放性交互式的全新网络，在此期间各色饮品由X牛酸酸乳房山分销点提供！");
document.writeln("·水乳交融 以死逼婚 成熟女性 众口交赞 上校鸡块");
document.writeln("<\/textarea>");
document.writeln("<table id=\"anniu\" width=\"100%\"><tr><!--[if IE]><td><input type=\"button\" onclick=\"get()\" value=\"粘贴进来\" class=\"pbtn1\" \/><\/td><td><div class=\"jiantou\">›<span class=\"STYLE3\">›<\/span><\/div><\/td><![endif]-->");
document.writeln("<td><input type=\"submit\" value=\"开始处理\" class=\"pbtn1\" \/><\/td>");
document.writeln("<td><span id=\"d_clip_container\" style=\"position:relative;display:none;\"><input type=\"button\" id=\"d_clip_button\" value=\"复制出去(需Flash Player9+)\" class=\"pbtn1\" \/><\/span><!--[if IE]><div class=\"jiantou\">›<span class=\"STYLE3\">›<\/span><\/div><\/td><td><input type=\"button\" id=\"d_clip_button_ie\" onClick=\"CopyForIE();\" value=\"复制出去\" class=\"pbtn1\" \/><![endif]--><\/td>");
document.writeln("<td width=\"100%\"><SPAN id=\"copyshow\"><\/SPAN><\/td><td><input id=\"CHundo\" type=button onclick=\"CHCommand.undo()\" title=\"撤销\"></td><td><input id=\"CHredo\" type=button onclick=\"CHCommand.redo()\" title=\"恢复\"><\/td><\/td><\/tr><\/table><\/td><\/tr><\/table><\/div>");

function get(){var wby = document.getElementById("co");var isValue=clipboardData.getData("text");wby.value=isValue}
/* 火星文转换 */
function qqgo(cc){
var str='';
for(var i=0;i<cc.length;i++){
if(charPYStr().indexOf(cc.charAt(i))!=-1)
str+=qqPYStr().charAt(charPYStr().indexOf(cc.charAt(i)));
else
str+=cc.charAt(i);
}
return str;
}
function convert_hxw(){document.getElementById('co').value=qqgo(document.getElementById('co').value);}
function charPYStr(){
return '颠煽逼官屠制裁硬奸摸舔狗妓娼嫖破奶坦治性毛江胡委分平宁革武思派法意自霸爸垃鄙宣社政主乳射淫禁新叶多合部客朋友激烈现象化食物事球睛同勃月允王皇帝每卡坐比印度尼西章黑向木母上肉玻碎死常福跟再冰封错流行注蛋版手忘失丹病衣服幼气小猪亲川火值星办元代背世出可真灰色光更音男味正叔太傻巫婆下安全天易如抱中华人民共追打湖南定张家界省亮走明泥土脏永玉少女寸井义令台幸青书贵乐言盲日甘左布北生白力老呀花守来楼毒喜犬马牛羊香泉田心鹿鱼贝飞士炎门风甜亚龙虎耳立呆山专业东丝严丧丰乡乱争亸众会伤伪体余侦傥党冢净刬剐剥务动劳勋勚协单厂厅历厉叆叇叹咤哌唝唡唣唿啮啰啴嗬嚯国场坏垅垯垱埝塆塬墙壸复奥妇妈妩姜娇婳宪尸屃届峃峣崄嵚干幺庆庼彟态总恶惊惧惨愦慭挜挦敌斗旸昽杀权枪梼梾槚檐歼残殴毁毙汇汉沓沨沵泽浉浐浕涛涢温游溇滪漤灭煺爷牦犟狝独猬玙玚玱珰珲琎璇畲疬瘆眬矿硁硙硚碹磙祎秽秾筜箓糇级纪纮纴纻纼绁组结绖统绤绦绬绹缊缐编缞网罚罢翙翚职腌腘腭臜艳艹茎荙药莼萚蒋虬螀蟏袅袆袯裆裈襕觃览觍詟训议讯记讱讻讼讽诇诐诪诱调谈谐谞谥谷豮贠贩贪贫贱贾贿赂赃资赍赑赒赕赖赗赚赟赪跖跶轪轰辀辌辒运进选邓郁酦巨钎钑钖钘钟钻铏铓铔铚铦铻锈锜锠锦锧锳锽镃镈镋镕镚镠镢镮镴镵闬闿阇阓阘阛阳阴阶陕韨领颋颎颒颕颙颣飏飐飔飖飗飚饤饦饳饸饹饻饾馂馃馉馌馎驲骃骉骍骎骔骕骙骦鱽鱾鲀鲄鲇鲉鲊鲌鲏鲓鲖鲗鲘鲙鲝鲪鲬鲯鲹鲾鲿鳀鳁鳂鳈鳉鳌鳑鳒鳛鳠鳡鳣鸟鸡鸤鸧鸮鸰鸴鸻鸼鹀鹇鹍鹐鹒鹓鹔鹖鹙鹝鹟鹠鹡鹢鹥鹯鹲鹴黄黡鼌鼗龁龂号阂贺横鸿红壶护户划话怀环换痪涣谎辉绘荤浑获祸击机积迹缉极辑几蓟剂济际夹钾价驾坚间艰检碱俭键涧浆讲酱胶脚绞较节晋烬荆鲸经静痉纠厩旧驹举据锯剧鹃绢决绝军骏凯颗壳课恳抠裤块侩宽况亏窥馈溃扩阔腊蓝滥琅涝镭垒泪离礼隶镰练凉辆谅疗辽镣猎鳞赁铃灵馏刘笼陇篓鲁滦论萝骡络侣虑绿骂吗脉满谩猫锚铆贸没镁们锰梦眯弥觅幂绵缅庙闽铭谬谋亩钠纳难闹内拟腻撵聂镍狞泞农诺疟呕沤盘庞抛赔喷鹏骗飘苹凭评颇铺朴谱栖齐骑岂启弃讫牵铅签谦钳潜堑呛抢锹桥侨翘窍窃钦寝倾穷区躯驱龋劝鹊确绕热韧认纫荣绒软锐润萨鳃赛伞扫涩刹纱晒删赡缮赏烧绍赊摄慑设绅婶肾渗声绳胜师狮诗时识驶势适饰试寿兽枢输赎术树竖数帅双谁税顺硕烁饲颂诵擞苏肃虽绥岁笋琐锁挞坛汤烫讨腾誊锑题屉条铁烃铜头秃图团颓蜕鸵椭袜湾顽万违围苇纬谓卫纹稳问窝卧乌诬芜吴坞雾误牺袭习铣细虾辖狭厦吓纤衔闲县线镶详响项萧销晓啸携写谢锌衅兴凶汹绣须许叙续轩悬癣绚学驯逊压鸭讶烟岩阎砚彦验鸯扬疡样谣页医铱颐遗仪蚁艺亿异绎荫银饮隐蝇赢颖佣痈踊咏优忧犹舆渔娱屿语狱誉预驭渊圆远约钥粤阅陨韵杂灾载赞凿枣贼赠综闸栅诈斋债毡崭栈战绽涨账胀赵辙锗这诊阵睁证执纸掷帜质滞肿诌轴皱骤诛烛嘱铸驻转妆状锥缀准着浊兹渍踪纵邹诅后伙秸杰诀夸里凌霉捻凄扦圣抬涂洼喂污锨咸蝎彝涌吁御愿岳云灶扎札筑于志座耗貉褐亨恒虹弘瑚唬互滑徊淮桓患豢慌恍回晦魂霍基稽肌姬吉棘嫉己祭悸寂忌枷加甲稼架嫁尖肩柬剪健疆匠焦郊嚼角酵街劫竭疥襟靳近浸兢晶京粳景敬竟究酒救居矩拒踞句炬捐卷爵菌郡咯楷磕科啃控酷胯筷快框眶盔傀愧括廓喇啦婪浪烙勒雷儡肋黎栗粒哩廉良僚寥撂劣霖淋陵硫留隆陋路潞卵略螺洛戮履率麻嘛埋漫莽茅矛貌煤媒媚盟猛孟迷秘密眠娩渺蔑敏螟命陌牡哪娜囊呢匿碾娘捏孽您凝扭糯虐哦藕偶潘乓胖裴沛片拼坪瓶坡剖埔浦欺崎祁起企器迄泣洽千仟黔前遣歉羌橇悄瞧撬俏怯侵禽情泅曲屈娶圈券瘸榷嚷惹忍任妊蓉容褥瑞撒腮塞嗓搔砂沙煞衫擅扇商稍邵奢赦娠沈甚慎升剩施拾屎逝噬侍室瘦蔬抒疏鼠述戍庶甩霜爽睡瞬舜朔司似松宋艘嗽宿算隋祟唆索塔坍潭探炭趟掏滔陶藤疼踢提啼剃挑跳帖筒投凸突湍推腿托陀瓦豌玩婉枉危桅惟尾渭慰瘟文吻紊翁斡屋吾侮戊勿悟吸檄席隙瞎霞暇夏仙舷嫌腺限相湘祥想享哮淆肖鞋斜泄芯信惺兄匈嗅袖需徐酗婿玄眩薛血旬汛迅押雅咽淹蜒演燕唁宴央秧佯洋仰妖舀耶也掖揖夷移椅以屹毅溢翌茵吟寅引英迎影映臃雍蛹幽悠由釉愚逾渝予雨宇欲育裕豫援源曰越砸哉宰咱糟藻燥曾宗眨炸摘窄瞻沾蘸占湛漳掌丈仗沼哲者蔗臻疹振蒸征之殖旨置稚痔忠衷仲洲粥咒宙朱竹拄蛀祝拽撰篆撞椎啄灼咨棕祖阻候活皆桔李羚酶苔途蛙胃掀歇姨泳油峪苑耘皂渣淤住0123456789０１２３４５６７８９零一二三四五六七八九十壹贰叁肆伍柒捌玖abcdefghijkmnoprtuwxyABCDEFGHIJKMNOPRTUWXYａｂｃｄｅｆｇｈｉｊｋｍｎｏｐｒｔｕｗｘｙ';}
function qqPYStr(){
return '癫桑Ｂ关涂致才应坚莫忝苟记苍瞟珀乃躺志信矛姜煳猥芬萍您格伍司哌珐易字坝坝菈庇萱设正煮洳设银劲噺旪哆匼蔀愙萠伖噭烮哯潒囮喰粅倳浗聙哃葧仴尣迋瑝渧烸鉲唑仳茚喥胒覀嶂嫼姠朩毋仩禸箥谇史瑺鍢哏洅栤葑諎锍荇紸蜑蝂掱莣夨丼并依抚呦気晓租儭〣吙徝煋かえ玳褙卋绌钶桢咴铯咣哽喑侽菋㊣菽呔儍莁嘙吓咹铨兲噫洳菢ф囮囚囻囲缒咑鍸喃萣涨镓堺渻煷赱朙苨汢赃咏玊尐囡団囲図囹囼圉圊淑圚圞讠吂曰咁咗咘丠苼苩仂咾吖婲垨逨喽蝳囍猋骉犇羴馫灥畾惢麤鱻贔飝壵燚闁闏憇亜龖虤聑竝槑屾钻嘢冬斯盐散峰箱孪桢嚲重汇三魏扌馀桢胆裆塚淨剗霍啵悟栋捞勳勩鞋单场听曆力靉靆歎吒呱嗊啢唕呼齧囉嘽呵谑蝈蝉蘾垄墶壋垫壪原牆壼複嗷付鎷辅薑胶嫿羡斯屭借嶨嶢嶮嶔杆么顷廎彠太种鄂金具厂凤憖掗撏迪豆暘曨沙拳腔檮棶檟簷间蝉鸥悔币彙汗遝渢濔折溮滻濜Ｔ溳翁油漊澦滥摘退嘢犛强獮读蝟璵瑒瑲璫珲璡璿佘鬁瘮矓框硜磑礄镟滚禕汇穠簹籙餱急寄紘紝紵紖絏煮杰絰捅綌絛緓綯縕线边縗往伐坝翽翬值醃膕齶臢豔艸今薘要蓴蘀讲虯螿蠨嫋褘襏档褌襴覎间覥讋逊意逊寄訒訩宋粉詗詖譸又掉堂鞋諝諡穀豶貟饭汤平溅甲会路沾支齎贔賙戝癞賵转贇赬蹠躂軑烘輈輬轀韵劲悬瞪鬱醱巨釺鈒鍚鈃锺鑽鉶鋩錏銍銛鋙鏽錡錩紧鑕鍈鍠鎡鎛钂鎔鏰鏐钁鐶鑞鑱閈闓闍闠闒闤羊音接陝韍铃頲熲頮頴顒纇颺颭颸颻飀飙飣飥飿餄餎餏餖餕餜餶饁餺馹駰驫騂駸騌驌騤驦魛魢魨魺鲶鮋鮓鮊鮍鮳鮦鰂鮜鱠鮺鮶鯒鯕鯵鰏鱨鯷鰮鰃鰁鱂鼇鰟鰜鰼鱯鱤鱣茑基鳲鶬鴞鴒鴬鴴鵃鵐鷳鶤鵮鶊鵷鷫鶡鶖鷊鶲鶹鶺鷁鷖鸇鸏鸘簧黶鼂鞀齕齗耗貉鹤亨虹弘瑚唬沪画化徊欢缓唤焕恍挥诲绘魂火货祸基稽饥绩吉棘挤己祭悸忌枷甲稼架监笺肩茧柬简见溅将奖匠焦侥缴轿劫靳近劲晶粳颈径究酒救居矩拒踞炬捐卷觉诀钧郡楷磕科客垦控库胯筷快旷况岿傀愧括廓蜡赖烂滥烙雷儡类狸鲤沥连炼粮两亮僚寥撂劣邻凛龄凌硫留咙拢搂虏孪纶论箩骆铝缕滤马嘛迈蛮漫莽茅矛貌煤媒闷盟猛孟谜秘密眠娩渺悯鸣命陌牡呐娜男恼馁尼匿碾捏镊柠拧浓糯虐藕偶潘乓胖裴沛朋片骗坪平瓶泼扑埔浦欺脐祁起企迄泣洽钎迁仟钱前谴枪强橇悄乔撬俏怯窃禽氢琼趋曲屈娶券却榷扰惹忍任妊蓉容阮瑞闰洒腮塞叁骚色杀沙筛山擅扇商稍邵奢赦射社娠审甚慎渗升剩圣失湿拾实屎逝噬释视守瘦蔬抒书属述戍庶甩霜爽睡瞬说朔似怂讼艘嗽诉算随碎损缩索獭滩炭趟陶藤疼踢提剃挑贴听同投凸突湍推腿脱驼瓦弯玩婉韦桅维尾渭慰闻吻紊涡斡钨污无吾侮戊悟锡檄席喜戏瞎霞侠下夏鲜贤舷献限厢祥想享象嚣淆肖挟谐泻芯信惺兄匈袖嘘徐酗绪续宣选眩薛寻讯迅鸦亚阉蜒颜厌唁谚央杨佯养窑也壹揖衣夷移椅以屹译翌茵吟寅引荧迎影拥臃雍蛹幽悠铀愚渝予与宇欲育裕豫鸳员缘曰跃岳悦匀晕砸哉宰暂糟藻泽曾宗铡眨炸摘窄瞻辗蘸占湛掌帐仗沼蛰者蔗疹镇挣郑殖旨挚置稚痔种洲粥咒昼诸竹瞩贮祝砖装壮椎坠谆啄灼浊自棕总纵祖候活皆桔决裤李羚酶撵七牵胜苔途蛙胃乌掀纤歇姨泳郁峪苑钥耘皂赠渣铸淤纸坐耗盒赫哼衡烘宏忽虎护猾槐怀环换痪荒幌蛔惠婚惑圾畸箕鸡缉极即脊伎剂寄既嘉家贾假价驾坚兼检俭件江讲礁交娇狡教秸截睫介津进禁烬荆睛鲸精警境靖揪九厩疽咀聚距俱惧剧眷倔均浚卡揩棵颗肯孔苦跨块侩狂矿亏魁馈困扩拉辣蓝朗酪涝乐累擂犁吏立璃镰粱聊燎了烈磷鳞灵榴馏窿漏露鹿滦掠萝落陆旅律妈骂吗慢忙猫毛帽霉眉妹檬锰梦糜米蜜棉勉秒妙皿明名寞拇拿那难淖你年念尿聂涅狞牛懦暖诺殴呕攀叛耪培佩偏瞥乒凭屏粕蒲普期畦祈骑乞砌气汽掐铅签乾钳潜欠腔抢敲桥鞘峭且钦擒氰酋蛆躯取去犬炔鹊攘绕人韧刃茸溶入蕊弱萨鳃桑丧莎刹啥煽陕汕伤捎哨绍舍深神婶肾牲盛狮石使誓嗜仕恃受兽殊淑黍术束墅衰拴双水吮顺硕私伺巳送搜擞溯蒜虽隧梭琐她汰痰碳叹淌烫涛淘特腾剔题蹄涕腆眺铁捅偷透秃兔团颓拖鸵娃外湾宛亡微违唯伪位尉卫蚊纹稳嗡我诬梧伍坞物务嘻犀袭系细匣辖厦先衔弦县陷线襄翔详响削宵校蝎邪懈薪心猩姓胸朽秀戌须蓄絮旋癣靴雪循殉逊压涯焉烟研衍堰雁焰殃鸯扬羊氧腰咬噎冶页医颐遗彝矣邑意益翼绎姻淫尹印蝇盈硬佣庸踊用优尤佑虞俞愉隅娱禹愈狱寓预原猿院约匝栽灾在遭凿灶憎踪闸乍诈宅寨粘展栈站彰张杖账找折辙锗砧枕震阵睁汁植只致秩炙盅钟重州诌帚皱蛛逐煮助注爪转赚妆状酌着兹鬃族诅厚豁接节理伶梅胎徒哇畏吓些宜咏犹喻远阅造喳迂筑о①②з④⑤б┐〥ɡо①②з④⑤б┐〥ɡо①②з④⑤б┐〥ɡ⑩①②з④⑤┐〥ɡɑвсÐёƒɡнΙ亅κΜΝοΡΥΤцωΧуɑвсÐёƒɡнΙ亅κΜΝοΡΥΤцωΧуɑвсÐёƒɡнΙ亅κΜΝοΡΥΤцωΧу';}
/* 强行分隔 */
function convert_fgf(){var s = document.getElementById("co").value.toString();var t = "";var code = (document.getElementById("fgf1").value);for(var i=0,l=s.length; i<l; i++){t += s.charAt(i) + code;}document.getElementById("co").value = t;}
/* 倒排字序 */
 function convert_dd() {
  var s = document.getElementById("co").value.toString();
  var arr = s.split("\n");
  var result = "";
  for ( var i = 0; i < arr.length; i++) {
   arr[i]=arr[i].replace(/(^[\s　]*)|([\s　]*$)/g,   "");
   arr[i]=arr[i].replace("\n","");
   if(arr[i]!="")
   {
      result += flipString(arr[i])+"\n";
   }   
  }
  document.getElementById("co").value = result;
 }
 function flipString(aString) {
  var last = aString.length - 1;
  var result = new Array(aString.length)
  for ( var i = last; i >= 0; --i) {
   var c = aString.charAt(i)
   var r = flipTable[c]
   result[last - i] = r != undefined ? r : c
  }
  return result.join('')
 }
 var flipTable = {
  '[' : ']',
  '【' : '】',
  '(' : ')',
  '（' : '）',
  '{' : '}',
  "\'" : ',',
  '<' : '>',
  '《' : '》',
  '“' : '”',
  '\r' : '\n'
 }
 for (i in flipTable) {
  flipTable[flipTable[i]] = i
 }
/* 竖排文字 */
var tblChars = [['┏','┓','┗','┛','┯','┷','┃','│', '━'],['╔','╗','╚','╝','╤','╧','║','│', '═'],['┌','┐','└','┘','┬','┴','│','┆', '─'],[' ',' ',' ',' ',' ',' ',' ',' ',  ' '],['','','','','','','│','│', ''],];
var tblTemplet = 1;var blankChar = '┊';var width=20;var height=8;
function convert1(){var s = document.getElementById("co").value.toString();s = s.replace(/\r/g, "");if(s.length == 0){document.getElementById("co").focus();alert("请首先输入要转换格式的文字。");return;}
var ary = [];var i,j, index;var t = "";index = 0;width = document.getElementById("x").value * 1;height = document.getElementById("y").value * 1;tblTemplet = document.getElementById("tbl").value * 1;for(i=width*2; i>=0; i--){ary[i] = new Array();}
while(index < s.length){for(i=width*2; i>=0; i--){for(j=0; j<=(height+1); j++){if( i == (width * 2)){if(j==0){ary[i][j] = tblChars[tblTemplet][1];}else if(j == (height + 1)){ary[i][j] = tblChars[tblTemplet][3];}else{ary[i][j] = tblChars[tblTemplet][6];}}else if( i== 0){
if(j==0){ary[i][j] = tblChars[tblTemplet][0];}else if(j == (height + 1)){ary[i][j] = tblChars[tblTemplet][2];}else{ary[i][j] = tblChars[tblTemplet][6];}}else if( i % 2 == 0){
if(j==0){ary[i][j] = tblChars[tblTemplet][4];}else if(j == (height + 1)){ary[i][j] = tblChars[tblTemplet][5];}else{ary[i][j] = tblChars[tblTemplet][7];}}else if(j == 0 || j == (height + 1)){ary[i][j] = tblChars[tblTemplet][8];}else{var c = getChar(s, index++);
if (c == '\n' || c == '\r'){/*if(j == 1){j = 0;continue;}else{*/while(j<(height+1)){ary[i][j] = blankChar;j++;}
j = height;
//}
}else{ary[i][j] = c;}}}}
for(j=0; j<=(height + 1); j++){
for(i=0; i<=width*2; i++){t += ary[i][j];}t += "\r\n";}t += "\r\n";}document.getElementById("co").value = t;}
var half = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','(',')','[',']','{','}','<','>','*','&','^','%','$','#','@','!','~','`','+','-','=','_','|','\\','\'','"',';',':','.',',','?','/',' ','（','）','【','】','《','》', '…', '—', '～', '“', '”', '‘', '’','「','」','『','』','【','】','《','》','〈','〉','（','）','｛','｝',' ','　','ā','á','ǎ','à','ē','é','ě','è','ī','í','ǐ','ì','ō','ó','ǒ','ò','ū','ú','ǔ','ù','ǚ','ǜ'];
var full = ['０','１','２','３','４','５','６','７','８','９','ａ','ｂ','ｃ','ｄ','ｅ','ｆ','ｇ','ｈ','ｉ','ｊ','ｋ','ｌ','ｍ','ｎ','ｏ','ｐ','ｑ','ｒ','ｓ','ｔ','ｕ','ｖ','ｗ','ｘ','ｙ','ｚ','Ａ','Ｂ','Ｃ','Ｄ','Ｅ','Ｆ','Ｇ','Ｈ','Ｉ','Ｊ','Ｋ','Ｌ','Ｍ','Ｎ','Ｏ','Ｐ','Ｑ','Ｒ','Ｓ','Ｔ','Ｕ','Ｖ','Ｗ','Ｘ','Ｙ','Ｚ','︵','︶','︻','︼','︷','︸','︽','︾','＊','＆','︿','％','＄','＃','＠','！','～','｀','＋','－','＝','＿','｜','＼','＇','＂','；','：','。','，','？','／', blankChar,'︵','︶','︻','︼','︽','︾', '┇', '│', '§','﹁','﹂','﹃','﹄','﹁','﹂','﹃','﹄','︻','︼','︽','︾','︿','﹀','︵','︶','︷','︸','┊','┊','ā.','á.','ǎ.','à.','ē.','é.','ě.','è.','ī.','í.','ǐ.','ì.','ō.','ó.','ǒ.','ò.','ū.','ú.','ǔ.','ù.','ǚ.','ǜ.'];
function getChar(s, index){if(index >= s.length){return blankChar;}
var c = s.charAt(index);for(var i=0; i<half.length; i++){if(c == half[i]){c = full[i]; }}return c;} 
/* 文章排版 */
function convert_paiban(){var body = "\n"+document.getElementById("co").value;body = body.replace(/ |　/ig,"");body = body.replace(/\r\n/ig,"\n");body = body.replace(/\n\n/ig,"\n");body = body.replace(/\n\n/ig,"\n");body = body.replace(/\n\n/ig,"\n");body = body.replace(/\n\n/ig,"\n");body = body.replace(/\n/ig,"\n\n");body = body.replace("\n\n","　　");body = body.replace(/\n/ig,"\n　　");document.getElementById("co").value=body;}
/* 非IE浏览器Flash复制 */
// Simple Set Clipboard System
// Author: Joseph Huckaby
var ZeroClipboard={version:"1.0.7",clients:{},moviePath:'ZeroClipboard.swf',nextId:1,$:function(thingy){if(typeof(thingy)=='string')thingy=document.getElementById(thingy);if(!thingy.addClass){thingy.hide=function(){this.style.display='none'};thingy.show=function(){this.style.display=''};thingy.addClass=function(name){this.removeClass(name);this.className+=' '+name};thingy.removeClass=function(name){var classes=this.className.split(/\s+/);var idx=-1;for(var k=0;k<classes.length;k++){if(classes[k]==name){idx=k;k=classes.length}}if(idx>-1){classes.splice(idx,1);this.className=classes.join(' ')}return this};thingy.hasClass=function(name){return!!this.className.match(new RegExp("\\s*"+name+"\\s*"))}}return thingy},setMoviePath:function(path){this.moviePath=path},dispatch:function(id,eventName,args){var client=this.clients[id];if(client){client.receiveEvent(eventName,args)}},register:function(id,client){this.clients[id]=client},getDOMObjectPosition:function(obj,stopObj){var info={left:0,top:0,width:obj.width?obj.width:obj.offsetWidth,height:obj.height?obj.height:obj.offsetHeight};while(obj&&(obj!=stopObj)){info.left+=obj.offsetLeft;info.top+=obj.offsetTop;obj=obj.offsetParent}return info},Client:function(elem){this.handlers={};this.id=ZeroClipboard.nextId++;this.movieId='ZeroClipboardMovie_'+this.id;ZeroClipboard.register(this.id,this);if(elem)this.glue(elem)}};ZeroClipboard.Client.prototype={id:0,ready:false,movie:null,clipText:'',handCursorEnabled:true,cssEffects:true,handlers:null,glue:function(elem,appendElem,stylesToAdd){this.domElement=ZeroClipboard.$(elem);var zIndex=99;if(this.domElement.style.zIndex){zIndex=parseInt(this.domElement.style.zIndex,10)+1}if(typeof(appendElem)=='string'){appendElem=ZeroClipboard.$(appendElem)}else if(typeof(appendElem)=='undefined'){appendElem=document.getElementsByTagName('body')[0]}var box=ZeroClipboard.getDOMObjectPosition(this.domElement,appendElem);this.div=document.createElement('div');var style=this.div.style;style.position='absolute';style.left=''+box.left+'px';style.top=''+box.top+'px';style.width=''+box.width+'px';style.height=''+box.height+'px';style.zIndex=zIndex;if(typeof(stylesToAdd)=='object'){for(addedStyle in stylesToAdd){style[addedStyle]=stylesToAdd[addedStyle]}}appendElem.appendChild(this.div);this.div.innerHTML=this.getHTML(box.width,box.height)},getHTML:function(width,height){var html='';var flashvars='id='+this.id+'&width='+width+'&height='+height;if(navigator.userAgent.match(/MSIE/)){var protocol=location.href.match(/^https/i)?'https://':'http://';html+='<object classid="clsid:d27cdb6e-ae6d-11cf-96b8-444553540000" codebase="'+protocol+'download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=9,0,0,0" width="'+width+'" height="'+height+'" id="'+this.movieId+'" align="middle"><param name="allowScriptAccess" value="always" /><param name="allowFullScreen" value="false" /><param name="movie" value="'+ZeroClipboard.moviePath+'" /><param name="loop" value="false" /><param name="menu" value="false" /><param name="quality" value="best" /><param name="bgcolor" value="#ffffff" /><param name="flashvars" value="'+flashvars+'"/><param name="wmode" value="transparent"/></object>'}else{html+='<embed id="'+this.movieId+'" src="'+ZeroClipboard.moviePath+'" loop="false" menu="false" quality="best" bgcolor="#ffffff" width="'+width+'" height="'+height+'" name="'+this.movieId+'" align="middle" allowScriptAccess="always" allowFullScreen="false" type="application/x-shockwave-flash" pluginspage="http://www.macromedia.com/go/getflashplayer" flashvars="'+flashvars+'" wmode="transparent" />'}return html},hide:function(){if(this.div){this.div.style.left='-2000px'}},show:function(){this.reposition()},destroy:function(){if(this.domElement&&this.div){this.hide();this.div.innerHTML='';var body=document.getElementsByTagName('body')[0];try{body.removeChild(this.div)}catch(e){}this.domElement=null;this.div=null}},reposition:function(elem){if(elem){this.domElement=ZeroClipboard.$(elem);if(!this.domElement)this.hide()}if(this.domElement&&this.div){var box=ZeroClipboard.getDOMObjectPosition(this.domElement);var style=this.div.style;style.left=''+box.left+'px';style.top=''+box.top+'px'}},setText:function(newText){this.clipText=newText;if(this.ready)this.movie.setText(newText)},addEventListener:function(eventName,func){eventName=eventName.toString().toLowerCase().replace(/^on/,'');if(!this.handlers[eventName])this.handlers[eventName]=[];this.handlers[eventName].push(func)},setHandCursor:function(enabled){this.handCursorEnabled=enabled;if(this.ready)this.movie.setHandCursor(enabled)},setCSSEffects:function(enabled){this.cssEffects=!!enabled},receiveEvent:function(eventName,args){eventName=eventName.toString().toLowerCase().replace(/^on/,'');switch(eventName){case'load':this.movie=document.getElementById(this.movieId);if(!this.movie){var self=this;setTimeout(function(){self.receiveEvent('load',null)},1);return}if(!this.ready&&navigator.userAgent.match(/Firefox/)&&navigator.userAgent.match(/Windows/)){var self=this;setTimeout(function(){self.receiveEvent('load',null)},100);this.ready=true;return}this.ready=true;this.movie.setText(this.clipText);this.movie.setHandCursor(this.handCursorEnabled);break;case'mouseover':if(this.domElement&&this.cssEffects){this.domElement.addClass('hover');if(this.recoverActive)this.domElement.addClass('active')}break;case'mouseout':if(this.domElement&&this.cssEffects){this.recoverActive=false;if(this.domElement.hasClass('active')){this.domElement.removeClass('active');this.recoverActive=true}this.domElement.removeClass('hover')}break;case'mousedown':if(this.domElement&&this.cssEffects){this.domElement.addClass('active')}break;case'mouseup':if(this.domElement&&this.cssEffects){this.domElement.removeClass('active');this.recoverActive=false}break}if(this.handlers[eventName]){for(var idx=0,len=this.handlers[eventName].length;idx<len;idx++){var func=this.handlers[eventName][idx];if(typeof(func)=='function'){func(this,args)}else if((typeof(func)=='object')&&(func.length==2)){func[0][func[1]](this,args)}else if(typeof(func)=='string'){window[func](this,args)}}}}};