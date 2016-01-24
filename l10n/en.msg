Unprocessed msg@Text: #{msg}
Yes icon@Text: #{icon} Yes
No icon@Text: #{icon} No
AddCardinal: Gain a cardinal
HeirDies: Heir dies
RulerDies: Ruler dies
LoseCardinal: Lose a cardinal
GainADM icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} administrative power
GainAT icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} army tradition
GainAuth amt@Double: #{gainOrLose amt} #{colourNum True amt} authority
GainBT icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} base tax
GainBP icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} base production
GainBM icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} base manpower
GainDIP icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} diplomatic power
GainDoom amt@Double: #{gainOrLose amt} #{colourNum False amt} doom
HeirGainClaim amt@Double: Heir #{gainsOrLoses amt} #{colourNum True amt} claim strength
GainDevotion icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} devotion
GainHordeUnity icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} horde unity
GainImperialAuthority icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} imperial authority
GainKarma icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} karma
GainLegitimacy icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} legitimacy
GainMIL icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} military power
GainNavyTradition icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} navy tradition
GainPapalInfluence icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} papal influence
GainPrestige icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} prestige
GainStability icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} stability
GainWarExhaustion icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum False amt} war exhaustion
GainYearlyManpower icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} #{plural amt "year's" "years'"} worth of manpower
GainADMSkill icon@Text amt@Double: Ruler #{gainsOrLoses amt} #{icon} #{colourNum True amt} administrative skill
GainDIPSkill icon@Text amt@Double: Ruler #{gainsOrLoses amt} #{icon} #{colourNum True amt} diplomatic skill
GainMILSkill icon@Text amt@Double: Ruler #{gainsOrLoses amt} #{icon} #{colourNum True amt} military skill
GainSiegeProgress amt@Double: #{gainOrLose amt} #{colourNum True amt} siege progress
GainPatAuth icon@Text amt@Double: #{gainOrLose amt} #{icon} #{reducedNum (colourPc True) amt} Patriarch authority
GainPiety icon@Text amt@Double: #{gainOrLose amt} #{icon} #{reducedNum (colourPc True) amt} Piety
GainRepTrad icon@Text amt@Double: #{gainOrLose amt} #{icon} #{reducedNum (colourNum True) amt} republican tradition
GainInflation icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourPc False amt} inflation
GainLocalAutonomy icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourPc False amt} local autonomy
ReformDesire icon@Text amt@Double: {{icon|catholic}} Catholicism has at least #{icon} #{reducedNum plainPc amt} reform desire
GainReformDesire icon@Text amt@Double: {{icon|catholic}} Catholicism #{gainsOrLoses amt} #{icon} #{reducedNum (colourPc False) amt} reform desire
GainMercantilism icon@Text amt@Double: #{gainOrLose amt} #{icon} #{reducedNum (colourPc True) amt} mercantilism
GainMP icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} manpower
GainMPFrac icon@Text amt@Double: #{gainOrLose amt} #{icon} manpower equal to #{reducedNum (colourPc True) amt} of maximum
Separatism icon@Text amt@Double: Has at least #{icon} #{roundNum amt} #{plural amt "year" "years"} of separatism
CountryMod: country modifier
ProvMod: province modifier
PermanentProvMod: permanent province modifier
RulerMod: ruler modifier
TradeMod: trade modifier
GainMod type@Text name@Text: Gain #{type} #{iquotes name}
GainModDur type@Text name@Text days@Double: Gain #{type} #{iquotes name} for #{roundNumNoSpace days} #{plural days "day" "days"}
GainModPow type@Text name@Text pow@Double: Gain #{type} #{iquotes name} (#{colourNumSign True pow} Power)
GainModPowDur type@Text name@Text pow@Double days@Double: Gain #{type} #{iquotes name} (#{colourNumSign True pow} Power) for #{plainNum days} #{plural days "day" "days"}
ActorGainsMod who@Text type@Text name@Text: #{who} gains #{type} #{iquotes name}
ActorGainsModDur who@Text type@Text name@Text days@Double: #{who} gains #{type} #{iquotes name} for #{plainNum days} #{plural days "day" "days"}
ActorGainsModPow who@Text type@Text name@Text pow@Double: #{who} gains #{type} #{iquotes name} (#{colourNumSign True pow} Power)
ActorGainsModPowDur who@Text type@Text name@Text pow@Double days@Double: #{who} gains #{type} #{iquotes name} (#{colourNumSign True pow} Power) for #{plainNum days} #{plural days "day" "days"}
HasModifier kind@Text name@Text: Has #{kind} #{iquotes name}
RemoveModifier kind@Text name@Text: Remove #{kind} ''"#{name}''
AllOf: All of:
OurCountry: Our country:
FROM: FROM:
PREV: PREV:
NoneOf: None of:
AtLeastOneOf: At least one of:
Area: Area containing this province:
AnyActiveTradeNode: Any trade node with a merchant present:
AnyAlly: Any ally:
AnyCoreCountry: Any country with a core on this province:
AnyCountry: Any country in the world:
AnyEnemyCountry: Any enemy country:
AnyKnownCountry: Any known country:
AnyNeighborCountry: Any neighbouring country:
AnyNeighborProvince: Any neighbouring province:
AnyOwnedProvince: Any owned province:
AnyRival: Any rival:
AnySubject: Any subject:
Capital: Capital:
Controller: Province controller:
Emperor: The Holy Roman Emperor:
AllCountries: All countries in the world:
EveryCountry: Every country in the world:
EveryEnemyCountry: Every enemy country:
EveryKnownCountry: Every known country:
EveryNeighborCountry: Every neighbouring country:
EveryNeighborProvince: Every neighbouring province:
EveryOwnedProvince: Every owned province:
EveryProvince: Every province in the world:
EveryRival: Every rival:
EverySubject: Every subject country:
HiddenEffect: Hidden effect:
If: If:
Limit: Limited to:
Owner: Province owner:
RandomActiveTradeNode: One random trade node with a merchant present:
RandomAlly: One random ally:
RandomCoreCountry: One random country with a core on this province:
RandomCountry: One random country:
RandomKnownCountry: One random known country:
RandomList: One of the following at random:
RandomNeighborCountry: One random neighbouring country:
RandomNeighborProvince: One random neighbouring province:
RandomOwnedProvince: One random owned province:
RandomProvince: One random province:
RandomRival: One random rival:
RandomChance chance@Double: #{plainPc chance} chance of:
Random: One of the following at random:
ChangeGovernment what@Text: Change government to #{what}
ContinentIs what@Text: Continent is #{what}
CultureIs what@Text: Culture is #{what}
CultureIsGroup what@Text: Culture is in #{what} group
RulerIsDynasty what@Text: Ruler is of #{what} dynasty
DisasterEnds what@Text: Disaster #{iquotes what} ends
GovernmentIs what@Text: Government is #{what}
HasAdvisor whom@Text: Has advisor #{whom}
HasAdvisorType icon@Text what@Text: Has #{icon} #{what} advisor
HasTerrain what@Text: Has #{what} terrain
CavalrySpawnsCountry whom@Text: A cavalry regiment loyal to #{whom} spawns
CavalrySpawnsProvince where@Text: A cavalry regiment spawns in #{where}
InfantrySpawnsCountry whom@Text: An infantry regiment loyal to #{whom} spawns
InfantrySpawnsProvince where@Text: An infantry regiment spawns in #{where}
AdvisorDies who@Text: Advisor #{who} dies
PrimaryCultureIs what@Text: Primary culture is #{what}
RegionIs what@Text: Province is in #{what} region
LoseAdvisor who@Text: Advisor #{who} leaves the country's court
RemoveFromEstate icon@Text whom@Text: Remove province from the #{icon} #{whom} estate
DisasterOngoing what@Text: The #{iquotes what} disaster is ongoing
ProvinceIs what@Text: Province is #{what}
Owns what@Text: Owns province #{what}
OwnsCore what@Text: Owns core province #{what}
Controls what@Text: Controls province #{what}
AdvisorExists advisorID@Double: Advisor with ID #{plainNum advisorID} exists
AdvisorIsEmployed advisorID@Double: Advisor with ID #{plainNum advisorID} is employed
ClearFlag flagType@Text name@Text: Clear #{flagType} flag <tt>#{name}</tt>
HasFlag flagType@Text name@Text: #{T.toTitle flagType} flag <tt>#{name}</tt> is set
SetFlag flagType@Text name@Text: Set #{flagType} flag <tt>#{name}</tt>
HadFlag category@Text name@Text days@Double: Has had #{category} flag #{name} for #{roundNum days} days
CountryFlag: country
ProvinceFlag: province
RulerFlag: ruler
GlobalFlag: global
HadCountryFlag _icon@Text name@Text days@Double: Has had country flag <tt>#{name}</tt> for at least #{roundNum days} #{plural days "day" "days"}
HadProvinceFlag _icon@Text name@Text days@Double: Has had province flag <tt>#{name}</tt> for at least #{roundNum days} #{plural days "day" "days"}
HadRulerFlag _icon@Text name@Text days@Double: Has had ruler flag <tt>#{name}</tt> for at least #{roundNum days} #{plural days "day" "days"}
HadGlobalFlag _icon@Text name@Text days@Double: Global flag <tt>#{name}</tt> has been set for at least #{roundNum days} #{plural days "day" "days"}
ColonySettlers amt@Double: Colony has at least #{roundNum amt} #{plural amt "settler" "settlers"}
WasAtWar amt@Double: Was at war within the last #{roundNum amt} #{plural amt "month" "months"}
HeirAge amt@Double: Heir is at least #{roundNum amt} #{plural amt "year" "years"} old
YearIs amt@Double: Year is #{roundNumNoSpace amt} or later
NumLoans amt@Double: Has at least #{plainNum amt} #{plural amt "loan" "loans"}
NumMercs amt@Double: Has at least #{roundNum amt} mercenary #{plural amt "regiment" "regiments"}
NumMissionaries icon@Text amt@Double: Has at least #{icon} #{roundNum amt} #{plural amt "missionary" "missionaries"}
NumPorts amt@Double: Has at least #{plainNum amt} #{plural amt "port" "ports"}
NumRebelArmies amt@Double: At least #{roundNum amt} rebel #{plural (round amt) "army is" "armies are"} present in the country
NumEmbargoes amt@Double: Is embargoing at least #{roundNum amt} other #{plural (round amt) "country" "countries"}
UnitsInProvince amt@Double: Province contains at least #{roundNum amt} #{plural (round amt) "regiment" "regiments"}
NumCities amt@Double: Owns at least #{roundNum amt} non-colony #{plural amt "province" "provinces"}
NumCitiesThan whom@Text: Owns at least as many cities as #{whom}
ToleranceToThis amt@Double: Tolerance to this religion is at least #{colourNum True amt}
RulerADM icon@Text amt@Double: Ruler's #{icon} administrative skill is at least #{roundNum amt}
ADMTech icon@Text amt@Double: #{icon} Administrative technology is at least #{roundNum amt}
ArmyTradition icon@Text amt@Double: #{icon} Army tradition is at least #{plainNum amt}
YearlyArmyTradition icon@Text amt@Double: #{icon} #{colourNumSign True amt} Yearly army tradition
BaseManpower icon@Text amt@Double: #{icon} Base manpower is at least #{roundNum amt}
BaseProduction icon@Text amt@Double: #{icon} Base production is at least #{roundNum amt}
BaseTax icon@Text amt@Double: #{icon} Base tax is at least #{roundNum amt}
CreateAdmiral icon@Text amt@Double: Gain #{icon} admiral with #{roundNum amt} navy tradition
CreateConquistador icon@Text amt@Double: Gain #{icon} conquistador with #{roundNum amt} navy tradition
CreateExplorer icon@Text amt@Double: Gain #{icon} explorer with #{roundNum amt} navy tradition
CreateGeneral icon@Text amt@Double: Gain #{icon} general with #{roundNum amt} army tradition
Development icon@Text development@Double: #{icon} Development is at least #{roundNum development}
RulerDIP icon@Text amt@Double: Ruler's #{icon} diplomatic skill is at least #{roundNum amt}
DIPTech icon@Text amt@Double: #{icon} Diplomatic technology is at least #{roundNum amt}
HordeUnity icon@Text amt@Double: #{icon} Horde unity is at least #{roundNum amt}
Karma icon@Text amt@Double: #{icon} Karma is at least #{colourPc True amt}
Legitimacy icon@Text amt@Double: #{icon} Legitimacy is at least #{roundPc amt}
YearlyLegitimacy icon@Text amt@Double: #{icon} #{colourPc True amt} Yearly legitimacy
RulerMIL icon@Text amt@Double: Ruler's #{icon} military skill is at least #{roundNum amt}
MILTech icon@Text amt@Double: #{icon} Military technology is at least #{roundNum amt}
NumAllies icon@Text amt@Double: Has at least #{icon} #{roundNum amt} #{plural amt "ally" "allies"}
NumCardinals icon@Text amt@Double: Has at least #{icon} #{roundNum amt} #{plural amt "cardinal" "cardinals"}
NumColonists icon@Text amt@Double: Has at least #{icon} #{roundNum amt} #{plural amt "colonist" "colonists"}
NumHeavyShips icon@Text amt@Double: Has at least #{icon} #{roundNum amt} heavy #{plural amt "ship" "ships"}
NumLightShips icon@Text amt@Double: Has at least #{icon} #{roundNum amt} light #{plural amt "ship" "ships"}
NumMerchants icon@Text amt@Double: Has at least #{icon} #{roundNum amt} #{plural amt "merchant" "merchants"}
Stability icon@Text amt@Double: #{icon} Stability is at least #{colourNumSign True amt}
TotalDevelopment icon@Text amt@Double: Country's total #{icon} development is at least #{roundNum amt}
TotalCardinals icon@Text amt@Double: At least #{icon} #{roundNum amt} #{plural amt "cardinal" "cardinals"} exist in the world
Unrest icon@Text amt@Double: Unrest is at least #{icon} #{colourNum False amt}
MonthlyIncome icon@Text amt@Double: #{icon} Monthly income is at least #{colourNum True amt} ducats
WarExhaustion icon@Text amt@Double: #{icon} War exhaustion is at least #{colourNum False amt}
WarScore icon@Text amt@Double: War score is at least #{icon} #{colourPc True amt}
RepTrad icon@Text amt@Double: #{icon} Republican tradition is at least #{reducedNum roundPc amt}
YearlyRepTrad icon@Text amt@Double: #{icon} #{reducedNum (colourPc True) amt} Yearly republican tradition
Inflation icon@Text amt@Double: #{icon} Inflation is at least #{colourPc False amt}
LocalAutonomy icon@Text amt@Double: #{icon} Local autonomy is at least #{colourPc False amt}
ManpowerPercentage icon@Text amt@Double: #{icon} Manpower reserves are at least #{reducedNum plainPc amt} of maximum
Mercantilism icon@Text amt@Double: #{icon} Mercantilism is at least #{reducedNum plainPc amt}
ChangeGoods icon@Text what@Text: Change province's goods produced to #{icon} #{what}
CreateAdvisor icon@Text what@Text: Gain #{icon} #{what} advisor
HasIdeaGroup icon@Text what@Text: Has activated #{icon} #{what}
ProducesGoods icon@Text what@Text: Produces #{icon} #{what}
HasEstate icon@Text what@Text: Has estate #{icon} #{what}
AssignToEstate icon@Text what@Text: Assign province to the #{icon} #{what} estate
RulerIsGeneral icon@Text _what@Text: Ruler is a #{icon} general
AlliedWith whom@Text: Is allied to #{whom}
CedeProvinceTo whom@Text: Cede province to #{whom}
ControlledBy whom@Text: Province is controlled by #{whom}
DefensiveWarAgainst whom@Text: Is defending in a war against #{whom}
DiscoverCountry whom@Text: Discover #{whom}
GainClaim who@Text: #{who} gains a claim on this province
GainPermanentClaimCountry who@Text: #{who} gains a permanent claim on this province
GainPermanentClaimProvince where@Text: Gain a permanent claim on #{where}
HasDiscovered whomOrWhere@Text: Has discovered #{whomOrWhere}
DiscoveredBy whom@Text: Has been discovered by #{whom}
Inherit whom@Text: Inherit #{whom}
Neighbors whom@Text: Has a border with #{whom}
IsSubjectOf whom@Text: Is a subject of #{whom}
LoseCoreCountry who@Text: #{who} loses their core on this province
LoseCoreProvince where@Text:  Lose core on #{where}
RoyalMarriageWith whom@Text: Has a royal marriage with #{whom}
OffensiveWarAgainst whom@Text: Is attacking in a war against #{whom}
OwnedBy whom@Text: Province is owned by #{whom}
ReleaseVassal whom@Text: Release #{whom} as a vassal
UnderSiegeBy whom@Text: Province is under siege by #{whom}
CountryIs who@Text: Is #{who}
TruceWith whom@Text: Has a truce with #{whom}
AtWarWith whom@Text: Is at war with #{whom}
MakeWhitePeace whom@Text: Make a white peace with #{whom}
CountryExists who@Text: #{who} exists
Exists yn@Bool: #{ifThenElseT yn "Exists" "Does ''not'' exist"}
SameReligion whom@Text: Religion is same as #{whom}
Religion icon@Text what@Text: Religion is #{icon} #{what}
SameReligionGroup whom@Text: Religion group is same as #{whom}
ReligionGroup _@Text what@Text: Religion is in #{what} group
ChangeSameReligion whom@Text: Change religion to that of #{whom}
ChangeReligion icon@Text what@Text: Change religion to #{icon} #{what}
IsCoreOf whom@Text: Is a core of #{whom}
HasCoreOn what@Text: Has a core on #{what}
HasClaim who@Text: #{who} has a claim on this province
HasClaimOn what@Text: Has a claim on #{what}
IsAIControlled yn@Bool: Is #{ifThenElseT yn "AI" "player"}-controlled
HasCardinal yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} a cardinal
HasHeir yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} an heir
HasOwnerCulture yn@Bool: Has #{ifThenElseT yn "the same culture as" "a different culture from"} its owner
HasOwnerReligion yn@Bool: Has #{ifThenElseT yn "the same religion as" "a different religion from"} its owner
HasPort yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} a port
HasSeatInParliament yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} a seat in Parliament
IsInRegency yn@Bool: Is#{ifThenElseT yn "" " ''not''"} in a regency
UnderSiege yn@Bool: Is#{ifThenElseT yn "" " ''not''"} under siege
AtWar yn@Bool: Is#{ifThenElseT yn "" " ''not''"} at war
IsCapital yn@Bool: Is#{ifThenElseT yn "" " ''not''"} the country's capital
IsCity yn@Bool: Is#{ifThenElseT yn "" " ''not''"} a city
IsColony yn@Bool: Is#{ifThenElseT yn "" " ''not''"} a colony
IsEmperor yn@Bool: Is#{ifThenElseT yn "" " ''not''"} the Holy Roman Emperor
IsFemale yn@Bool: Is #{ifThenElseT yn "female" "male"}
IsLesserInUnion yn@Bool: Is#{ifThenElseT yn "" " ''not''"} a lesser partner in a personal union
IsLooted yn@Bool: Is#{ifThenElseT yn "" " ''not''"} looted
IsOverseas yn@Bool: Is#{ifThenElseT yn "" " ''not''"} overseas
IsPartOfHRE yn@Bool: Is#{ifThenElseT yn "" " ''not''"} part of the Holy Roman Empire
IsCenterOfReformation yn@Bool: Is#{ifThenElseT yn "" " ''not''"} a Center of Reformation
IsSubject yn@Bool: Is#{ifThenElseT yn "" " ''not''"} a subject nation
PapacyIsActive yn@Bool: Papal interaction is#{ifThenElseT yn "" " ''not''"} active
HasBeenPlayer yn@Bool: Has#{ifThenElseT yn "" " ''never''"} been player-controlled
GainCB cbtype@Text whom@Text: Gain #{cbtype} casus belli against #{whom}
GainCBDuration cbtype@Text whom@Text months@Double: Gain #{cbtype} casus belli against #{whom} for #{roundNum months} months
ReverseGainCB cbtype@Text who@Text: #{who} gains #{cbtype} casus belli against this country
ReverseGainCBDuration cbtype@Text who@Text months@Double: #{who} gains #{cbtype} casus belli against this country for #{roundNum months} months
FactionGainInfluence icon@Text whom@Text amt@Double: #{gainOrLose amt} #{icon} #{plainNum amt} influence for the #{whom} faction
FactionInPower icon@Text whom@Text: #{icon} #{whom} is in power
HasFactions yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} factions
HasBuilding icon@Text what@Text: Has #{icon} #{what} building
Indefinitely: indefinitely
ForDays days@Double: for #{roundNum days} days
EstateLoyalty icon@Text who@Text amt@Double: #{icon} #{who} has at least #{plainNum amt} loyalty
EstateInfluence icon@Text who@Text amt@Double: #{icon} #{who} has at least #{plainNum amt} influence
AddEstateLoyalty icon@Text who@Text amt@Double: #{icon} #{who} #{gainsOrLoses amt} #{plainNum amt} loyalty
AddEstateInfluence icon@Text who@Text amt@Double: #{icon} #{who} #{gainsOrLoses amt} #{plainNum amt} influence
EstateLoyaltyModifier icon@Text who@Text what@Text amt@Double dur@Text: #{icon} #{who} gains loyalty modifier #{iquotes what} (#{colourNum True amt} loyalty) #{dur}
EstateInfluenceModifier icon@Text who@Text what@Text amt@Double dur@Text: #{icon} #{who} gains influence modifier #{iquotes what} (#{colourNum True amt} influence) #{dur}
AddOpinion what@Text whom@Text: Gain opinion modifier #{iquotes what} towards #{whom}
ReverseAddOpinion what@Text who@Text: #{who} gains opinion modifier #{iquotes what} towards this country
AddOpinionDur what@Text whom@Text years@Double: Gain opinion modifier #{iquotes what} towards #{whom} for #{plainNum years} years
ReverseAddOpinionDur what@Text who@Text years@Double: #{who} gains opinion modifier #{iquotes what} towards this country for #{plainNum years} years
HasOpinionMod what@Text whom@Text: Has opinion modifier #{iquotes what} towards #{whom}
RemoveOpinionMod what@Text whom@Text: Lose opinion modifier #{iquotes what} towards #{whom}
AddTreasury icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} ducats
AddYearsOfIncome icon@Text amt@Double: #{gainOrLose amt} #{icon} ducats equal to #{colourNum True amt} #{plural amt "year" "years"} of income
NewHeir: Gain a new heir
NewHeirClaim claim@Double: Gain a new heir with claim strength #{plainNum claim}
NewHeirDynasty flag@Text: Gain a new heir of the same dynasty as #{flag}
NewHeirDynastyClaim flag@Text claim@Double: Gain a new heir with claim strength #{roundNum claim}
NewHeirAge age@Double: Gain a new #{roundNum age} year old heir
NewHeirAgeClaim age@Double claim@Double: Gain a new #{roundNum age} year old heir with claim strength #{roundNum claim}
NewHeirAgeFlag age@Double flag@Text: Gain a new #{roundNum age} year old heir of the same dynasty as #{flag}
NewHeirAgeFlagClaim age@Double flag@Text claim@Double: Gain a new #{roundNum age} year old heir of the same dynasty as #{flag} with claim strength #{roundNum claim}
BuildToForcelimitLand inficon@Text infantry@Double cavicon@Text cavalry@Double articon@Text artillery@Double: Build land units to forcelimit: #{inficon} #{reducedNum plainPc infantry} infantry, #{cavicon} #{reducedNum plainPc cavalry} cavalry, #{articon} #{reducedNum plainPc artillery} artillery
BuildToForcelimitNavy heavyicon@Text heavy@Double lighticon@Text light@Double gallicon@Text galley@Double transpicon@Text transport@Double: Build naval units to forcelimit: #{heavyicon} #{reducedNum plainPc heavy} heavy ships, #{lighticon} #{reducedNum plainPc light} light ships, #{gallicon} #{reducedNum plainPc galley} galleys, #{transpicon} #{reducedNum plainPc transport} transports
ProvinceEvent: province event
CountryEvent: country event
TriggerEvent evttype@Text evtid@Text name@Text: Trigger #{evttype} #{iquotes name} <!-- #{evtid} -->
TriggerEventDays evttype@Text evtid@Text name@Text days@Double: Trigger #{evttype} #{iquotes name} <!-- #{evtid} --> in #{roundNum days} days
DeclareWarWithCB whom@Text cb@Text: Declare war on #{whom} using #{cb} casus belli
GainAdvisor skill@Double: Gain skill #{roundNum skill} advisor
GainAdvisorDiscount skill@Double: Gain skill #{roundNum skill} advisor (50% cheaper to employ)
GainAdvisorLoc where@Text skill@Double: Gain skill #{roundNum skill} advisor in #{where}
GainAdvisorLocDiscount where@Text skill@Double: Gain skill #{roundNum skill} advisor in #{where} (50% cheaper to employ)
GainAdvisorName name@Text skill@Double: Gain skill #{roundNum skill} advisor named #{name}
GainAdvisorNameDiscount name@Text skill@Double: Gain skill #{roundNum skill} advisor named #{name} (50% cheaper to employ)
GainAdvisorNameLoc name@Text where@Text skill@Double: Gain skill #{roundNum skill} advisor named #{name} in #{where}
GainAdvisorNameLocDiscount name@Text where@Text skill@Double: Gain skill #{roundNum skill} advisor named #{name} in #{where} (50% cheaper to employ)
GainAdvisorType advtype@Text skill@Double: Gain skill #{roundNum skill} #{advtype} advisor
GainAdvisorTypeDiscount advtype@Text skill@Double: Gain skill #{roundNum skill} #{advtype} advisor (50% cheaper to employ)
GainAdvisorTypeLoc advtype@Text where@Text skill@Double: Gain skill #{roundNum skill} #{advtype} advisor in #{where}
GainAdvisorTypeLocDiscount advtype@Text where@Text skill@Double: Gain skill #{roundNum skill} #{advtype} advisor in #{where} (50% cheaper to employ)
GainAdvisorTypeName advtype@Text name@Text skill@Double: Gain skill #{roundNum skill} #{advtype} advisor named #{name}
GainAdvisorTypeNameDiscount advtype@Text name@Text skill@Double: Gain skill #{roundNum skill} #{advtype} advisor named #{name} (50% cheaper to employ)
GainAdvisorTypeNameLoc advtype@Text name@Text where@Text skill@Double: Gain skill #{roundNum skill} #{advtype} advisor named #{name} in #{where}
GainAdvisorTypeNameLocDiscount advtype@Text name@Text where@Text skill@Double: Gain skill #{roundNum skill} #{advtype} advisor named #{name} in #{where} (50% cheaper to employ)
GainFemaleAdvisor female@Bool skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor
GainFemaleAdvisorDiscount female@Bool skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor (50% cheaper to employ)
GainFemaleAdvisorLoc female@Bool where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor in #{where}
GainFemaleAdvisorLocDiscount female@Bool where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor in #{where} (50% cheaper to employ)
GainFemaleAdvisorName female@Bool name@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor named #{name}
GainFemaleAdvisorNameDiscount female@Bool name@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor named #{name} (50% cheaper to employ)
GainFemaleAdvisorNameLoc female@Bool name@Text where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor named #{name} in #{where}
GainFemaleAdvisorNameLocDiscount female@Bool name@Text where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor named #{name} in #{where} (50% cheaper to employ)
GainFemaleAdvisorType female@Bool advtype@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} advisor
GainFemaleAdvisorTypeDiscount female@Bool advtype@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} advisor (50% cheaper to employ)
GainFemaleAdvisorTypeLoc female@Bool advtype@Text where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} advisor in #{where}
GainFemaleAdvisorTypeLocDiscount female@Bool advtype@Text where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} advisor in #{where} (50% cheaper to employ)
GainFemaleAdvisorTypeName female@Bool advtype@Text name@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} advisor named #{name}
GainFemaleAdvisorTypeNameDiscount female@Bool advtype@Text name@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} advisor named #{name} (50% cheaper to employ)
GainFemaleAdvisorTypeNameLoc female@Bool advtype@Text name@Text where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} advisor named #{name} in #{where}
GainFemaleAdvisorTypeNameLocDiscount female@Bool advtype@Text name@Text where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} advisor named #{name} in #{where} (50% cheaper to employ)
RebelLeaderRuler: The leader of the rebels becomes the country's new ruler
NewRuler regent@Bool: A new #{ifThenElseT regent "regent" "ruler"} comes to power
NewRulerLeader regent@Bool name@Text: The leader #{name} becomes #{ifThenElseT regent "regent" "ruler"}
NewRulerAttribs regent@Bool: A new #{ifThenElseT regent "regent" "ruler"} comes to power with the following attributes:
NewRulerLeaderAttribs regent@Bool name@Text: The leader #{name} becomes #{ifThenElseT regent "regent" "ruler"} with the following attributes:
LeaderRuler regent@Bool name@Text: The leader #{name} comes to power as #{ifThenElseT regent "regent" "ruler"}
NewRulerName name@Text: Named #{name}
NewRulerDynasty name@Text: Of the #{name} dynasty
NewRulerAge amt@Double: Aged #{roundNum amt} years
NewRulerAdm fixed@Bool amt@Double: #{ifThenElseT fixed "Fixed " ""}{{icon|adm}} #{roundNum amt} administrative skill
NewRulerDip fixed@Bool amt@Double: #{ifThenElseT fixed "Fixed " ""}{{icon|dip}} #{roundNum amt} diplomatic skill
NewRulerMil fixed@Bool amt@Double: #{ifThenElseT fixed "Fixed " ""}{{icon|mil}} #{roundNum amt} military skill
NewRulerClaim amt@Double: Claim strength #{roundNum amt}
NewRulerFixed adm@Double dip@Double mil@Double: A new ruler comes to power with fixed skills {{icon|adm}} #{roundNum adm}, {{icon|dip}} #{roundNum dip}, {{icon|mil}} #{roundNum mil}
EstateHasInfluenceModifier icon@Text estate@Text modifier@Text: #{icon} #{estate} estate has influence modifier #{iquotes modifier}
TriggerSwitch: The first of the following that is true:
TriggerSwitchClause cond@Text: If #{cond}:
ProvinceHasRebels icon@Text rtype@Text: Province's most likely rebel type is #{icon} #{rtype}
RebelsFriendlyTo friend@Text: friendly to #{friend}
RebelsLedBy leader@Text: led by #{leader}
RebelsGainProgress amt@Double: gaining #{roundNum amt} progress towards their next uprising
SpawnRebels rtype@Text size@Double friend@Text leader@Text win@Bool progress@Text: #{rtype} (size #{roundNum size})#{friend} rise in revolt#{leader}#{ifThenElseT win " and occupy the province" ""}#{progress}
RebelsHaveRisen icon@Text rtype@Text: #{icon} #{rtype} have risen in revolt
TagGainsCore who@Text: #{who} gains a core on this province
GainCoreOnProvince prov@Text: Gain core on #{prov}
HasDLC icon@Text dlc@Text: DLC #{icon} #{dlc} is active
Province where@Text: Province #{where}:
TechGroup icon@Text name@Text: Technology group is #{icon} #{name}
NumOfReligion icon@Text name@Text amt@Double: Has at least #{roundNum amt} #{plural (round amt) "province" "provinces"} of #{icon} #{name} religion
StrongestTradePower who@Text: #{who} is the strongest trade power in this node
AreaIs what@Text: Is in #{what} area
DominantReligion icon@Text what@Text: The country's dominant religion is #{icon} #{what}
HREReligion icon@Text what@Text: The Holy Roman Empire's official religion is #{icon} #{what}
SetHREReligionLocked yn@Bool: The Holy Roman Empire's religion #{ifThenElseT yn "becomes fixed" "is no longer fixed"}
SetHREReligion icon@Text what@Text: The Holy Roman Empire's official religion becomes #{icon} #{what}
SetHREHereticReligion icon@Text what@Text: The #{icon} #{what} religion is considered heretical within the Holy Roman Empire
SignWestphalia: From now on, the Emperor, Electors and Princes of the {{icon|hre}} Holy Roman Empire may be of any Christian religion without being considered heretics.
HRELeaguesEnabled yn@Bool: Leagues for the religion of the Holy Roman Empire #{ifThenElseT yn "have" "have ''not''"} begun to form
HREReligionLocked yn@Bool: The religion of the Holy Roman Empire #{ifThenElseT yn "is" "is ''not''"} fixed
HREWestphalia yn@Bool: The Peace of Westphalia #{ifThenElseT yn "has" "has ''not''"} been signed
IsElector yn@Bool: Is#{ifThenElseT yn "" " ''not''"} an Elector of the Holy Roman Empire
NoHREReforms: No reforms have been passed in the Holy Roman Empire
HREPassedReform what@Text: The imperial reform ''#{what}'' has been passed
EnableHRELeagues: Leagues for the religion of the Holy Roman Empire begin to form.
IsInLeagueWar yn@Bool: Is#{ifThenElseT yn "" " ''not''"} involved in a religious league war
IsLeagueEnemy whom@Text: Is in the opposite religious league to #{whom}
ReligionYears icon@Text name@Text years@Double: The #{icon} #{name} religion has existed for at least #{plainNum years} years
HasIdea what@Text: Has idea #{iquotes what}
ReligionProvinces icon@Text name@Text amt@Double: Country has at least #{roundNum amt} #{plural (round amt) "province" "provinces"} of #{icon} #{name} religion
GoodsProvinces icon@Text name@Text amt@Double: Country has at least #{roundNum amt} #{plural (round amt) "province" "provinces"} producing #{icon} #{name}
HasAristocraticIdea name@Text num@Int: Has {{icon|aristocratic}} Aristocratic idea #{show num} #{iquotes name}
HasEconomicIdea name@Text num@Int: Has {{icon|economic}} Economic idea #{show num} #{iquotes name}
HasDefensiveIdea name@Text num@Int: Has {{icon|defensive}} Defensive idea #{show num} #{iquotes name}
HasInnovativeIdea name@Text num@Int: Has {{icon|innovative}} Innovative idea #{show num} #{iquotes name}
HasOffensiveIdea name@Text num@Int: Has {{icon|offensive}} Offensive idea #{show num} #{iquotes name}
HasMaritimeIdea name@Text num@Int: Has {{icon|maritime}} Maritime idea #{show num} #{iquotes name}
Colonists icon@Text amt@Double: #{icon} #{colourNumSign True amt} Colonists
MayExplore: {{icon|may explore|28px}} Can recruit explorers and conquistadors. Explorers may explore ocean provinces.
GainColonialRange icon@Text amt@Double: #{icon} #{colourNumSign True amt} Colonial range
GlobalSettlers icon@Text amt@Double: #{icon} #{colourNumSign True amt} Global settler increase
GlobalTariffs icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Global tariffs
NavalForcelimitMod icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Naval forcelimit modifier
GainOverseasCB: Gain permanent "Overseas Expansion" [[Casus Belli]] against countries with Indian, Sub-Saharan, Chinese or Nomad tech group. (Only usable if country's tech group is Western, Eastern or Anatolian.)
GainPrimitivesCB: Gain permanent "Colonial Conquest" [[Casus Belli]] against all primitives.
NavyTradition icon@Text amt@Double: #{icon} Naval tradition is at least #{plainNum amt}
YearlyNavyTradition icon@Text amt@Double: #{icon} #{colourNumSign True amt} Yearly naval tradition
HeavyShipCombatAbility icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Heavy ship combat ability
LightShipCombatAbility icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Light ship combat ability
GalleyCombatAbility icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Galley combat ability
GlobalShipRepair icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Ship repair
GlobalShipCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Ship cost
RegimentCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Regiment cost
NavalLeaderManeuver icon@Text amt@Double: #{icon} #{colourNumSign True amt} Naval leader maneuver
BlockadeEfficiency icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Blockade efficiency
GainSeaRepair: Ships repair while in coastal sea provinces.
Primitives yn@Bool: Is#{ifThenElseT yn "" " ''not''"} primitive
GlobalTaxModifier icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Global tax modifier
BuildCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Build cost
YearlyInflationReduction icon@Text amt@Double: #{icon} #{colourNumSign False (negate amt)} Yearly inflation reduction
InterestPerAnnum icon@Text amt@Double: #{icon} #{colourNumSign False amt} Interest per annum
GlobalAutonomy icon@Text amt@Double: #{icon} #{colourNumSign False amt} Monthly autonomy change
LandMaintenanceMod icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Land maintenance modifier
NavalMaintenanceMod icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Naval maintenance modifier
ProdEff icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Production efficiency
DevelCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} {{DLC-only|Development cost}}
GainReligiousCB: {{icon|cb on religious enemies|28px}} Gain permanent "Holy War" and "Purging of Heresy" [[Casus Belli]] against heathens and heretics respectively.
Missionaries icon@Text amt@Double: #{icon} #{colourNumSign True amt} Missionaries
StabilityCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Stability cost modifier
MissionaryStrength icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Missionary strength
ToleranceHeathen icon@Text amt@Double: #{icon} #{colourNumSign True amt} Tolerance of heathens
ToleranceHeretic icon@Text amt@Double: #{icon} #{colourNumSign True amt} Tolerance of heretics
ToleranceTrue icon@Text amt@Double: #{icon} #{colourNumSign True amt} Tolerance of the true faith
YearlyPapalInfluence icon@Text amt@Double: #{icon} #{colourNumSign True amt} Yearly papal influence
YearlyDevotion icon@Text amt@Double: #{icon} #{colourNumSign True amt} {{DLC-only|Yearly devotion}}
MonthlyFervor icon@Text amt@Double: #{icon} #{colourNumSign True amt} {{DLC-only|Monthly fervor}}
ChurchPowerModifier icon@Text amt@Double: #{icon} #{colourNumSign True amt} {{DLC-only|Church power}}
Prestige icon@Text amt@Double: #{icon} Prestige is at least #{colourNumSign True amt}
YearlyPrestige icon@Text amt@Double: #{icon} #{colourNumSign True amt} Yearly prestige
MissionaryStrengthVsHeretics icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Missionary strength vs heretics
CultureConvCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Culture conversion cost
HasOpinion amt@Double whom@Text: Has at least #{colourNumSign True amt} opinion of #{whom}
NormalOrHistoricalNations yn@Bool: #{ifThenElseT yn "Playing" "''Not'' playing"} with normal or historical nations
IsCustomNation yn@Bool: Is#{ifThenElseT yn "" " ''not''"} playing a custom nation
ReligionEnabled icon@Text what@Text: The #{icon} #{what} religion is enabled
CapitalIs what@Text: Capital is #{what}
FullIdeaGroup icon@Text what@Text: Has completed #{icon} #{what}
TradeIncomePercentage amt@Double: Trade accounts for at least #{reducedNum plainPc amt} of national income
ReligiousUnity icon@Text amt@Double: #{icon} <!-- idea: #{reducedNum (colourPcSign True) amt} --> Religious unity is at least #{reducedNum (colourPc True) amt}
HasADM icon@Text amt@Double: Has at least #{icon} #{plainNum amt} administrative power
HasDIP icon@Text amt@Double: Has at least #{icon} #{plainNum amt} diplomatic power
HasMIL icon@Text amt@Double: Has at least #{icon} #{plainNum amt} military power
RankDuchy: Government rank is at least {{icon|duchy}} Duchy
RankKingdom: Government rank is {{icon|kingdom}}Kingdom or {{icon|empire}} Empire<!-- not {{icon|duchy}} Duchy -->
RankEmpire: Government rank is {{icon|empire}} Empire<!-- not {{icon|duchy}} Duchy or {{icon|kingdom}} Kingdom -->
SetRankDuchy: Set government rank to {{icon|duchy}} Duchy
SetRankKingdom: Set government rank to {{icon|kingdom}} Kingdom
SetRankEmpire: Set government rank to {{icon|empire}} Empire
Overextension icon@Text amt@Double: Has at least #{icon} #{reducedNum plainPc amt} overextension
RandomNewWorld yn@Bool: Is#{ifThenElseT yn "" " ''not''"} playing with a Random New World
IsColonialNation yn@Bool: Is#{ifThenElseT yn "" " ''not''"} a colonial nation
IsFormerColonialNation yn@Bool: Is#{ifThenElseT yn "" " ''not''"} a former colonial nation
IsNomad yn@Bool: Is#{ifThenElseT yn "" " ''not''"} a steppe horde
ReligionReformed yn@Bool: The country's religion has#{ifThenElseT yn "" " ''not''"} been reformed
ChangeTag who@Text: Country becomes #{who}
SetInEmpire yn@Bool: Province #{ifThenElseT yn "joins" "leaves"} the Holy Roman Empire
HasSecondaryReligion yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} a secondary religion
SecondaryReligion icon@Text what@Text: Secondary religion is #{icon} #{what}
IsDefenderOfFaith yn@Bool: Is#{ifThenElseT yn "" " ''not''"} Defender of the Faith
LegitimacyOrHordeUnity amt@Double: {{icon|legitimacy}} Legitimacy or {{icon|horde unity}} Horde unity is at least #{roundNum amt}
CheckVariable _icon@Text what@Text amt@Double: Value of variable #{what} is at least #{plainNum amt}
ChangeTechGroup icon@Text what@Text: Change technology group to #{icon} #{what}
ChangeUnitType icon@Text what@Text: Change units to #{icon} #{what}
NoBaseWeight: (no base weight?)
AIBaseWeight amt@Double: Base weight: #{plainNum amt}
AIFactorOneline factor@Text multiplier@Double: #{factor}: #{bold (plainNum multiplier)}
AIFactorHeader multiplier@Double: #{bold (plainNum multiplier)}:
Lucky yn@Bool: Is#{ifThenElseT yn "" " ''not''"} a lucky nation
HasArtistLevel icon@Text level@Double: Has a level #{roundNum level} #{icon} Statesman advisor
HasDiplomatLevel icon@Text level@Double: Has a level #{roundNum level} #{icon} Statesman advisor
HasArmyReformerLevel icon@Text level@Double: Has a level #{roundNum level} #{icon} Army Reformer advisor
HasNaturalScientistLevel icon@Text level@Double: Has a level #{roundNum level} #{icon} Natural Scientist advisor
HasNavyReformerLevel icon@Text level@Double: Has a level #{roundNum level} #{icon} Navy Reformer advisor
HasTheologianLevel icon@Text level@Double: Has a level #{roundNum level} #{icon} Theologian advisor
HasTraderLevel icon@Text level@Double: Has a level #{roundNum level} #{icon} Trader advisor
HasStatesmanLevel icon@Text level@Double: Has a level #{roundNum level} #{icon} Statesman advisor
NumRoyalMarriages icon@Text amt@Double: Has at least #{icon} #{roundNum amt} royal #{plural amt "marriage" "marriages"}
IsBankrupt yn@Bool: Is#{ifThenElseT yn "" " ''not''"} bankrupt
NumColonialSubjects amt@Double: Has at least #{roundNum amt} colonial #{plural amt "subject" "subjects"}
TradeEfficiency icon@Text amt@Double: #{icon} #{reducedNum (colourPc True) amt} <!-- Has at least #{reducedNum plainPc amt} --> trade efficiency
HasWarTaxes yn@Bool: Has#{ifThenElseT yn "" " ''not''"} raised war taxes
RevoltPercentage amt@Double: At least #{roundPc amt} of provinces are in revolt
HasAnyDisaster yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} an ongoing disaster
HasActivePolicy what@Text: Has enacted the policy #{iquotes what}
HasDucats icon@Text amt@Double: Has at least #{icon} #{plainNum amt} ducats
HasParliament yn@Bool: The country #{ifThenElseT yn "has" "does ''not'' have"} a parliament
HasTruce yn@Bool: The country #{ifThenElseT yn "has" "does ''not'' have"} a truce with another country
NumRebelControlledProvinces amt@Double: At least #{plainNum amt} #{plural amt "province is" "provinces are"} controlled by rebels
FortLevel icon@Text amt@Double: #{icon} Fort level is at least #{roundNum amt}
HasTradeModifier _icon@Text who@Text what@Text: #{who} has trade modifier #{iquotes what}
IsMonth what@Text: Month is #{what}
IsSea yn@Bool: Is#{ifThenElseT yn "" " ''not''"} sea
HeavyShip whom@Text: Create a {{icon|heavy ship}} heavy ship belonging to #{whom}
LightShip whom@Text: Create a {{icon|light ship}} light ship belonging to #{whom}
Galley whom@Text: Create a {{icon|galley}} galley belonging to #{whom}
NumColonies amt@Double: Has at least #{roundNum amt} #{plural amt "colony" "colonies"}
ChangeCulture what@Text: Change culture to #{what}
NavalForcelimit icon@Text amt@Double: #{icon} Naval forcelimit is at least #{plainNum amt}
Blockade icon@Text amt@Double: Is at least #{icon} #{plainPc amt} blockaded
CreateAlliance whom@Text: Create alliance with #{whom}
AddLocalUnrest icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum False amt} unrest
GoldIncomePercentage icon@Text amt@Double: At least #{reducedNum plainPc amt} of the country's income is from #{icon} Gold
IsTribal yn@Bool: Is#{ifThenElseT yn "" " ''not''"} tribal
SetCapital what@Text: Change capital to #{what}
ChangePrimaryCulture what@Text: Change primary culture to #{what}
ColonialRegion where@Text: Is in colonial region #{where}
JuniorUnionWith whom@Text: Is the junior partner in a personal union with #{whom}
SeniorUnionWith whom@Text: Is the senior partner in a personal union with #{whom}
VassalOf whom@Text: Is a vassal of #{whom}
OverlordOf whom@Text: Is the overlord of #{whom}
ChangeProvinceName what@Text: Rename province to #{what}
RenameCapital what@Text: Rename provincial capital to #{what}
OwnsOrVassal where@Text: Owns #{where} or is overlord of its owner
IsInColonialRange whom@Text: Is within colonial range of #{whom}
ConstructingGreatProject what@Text: Is constructing the #{what}
Constructing what@Text: Is constructing a #{what}
StartConstructingGreatProject what@Text: Begin constructing the #{what}
CancelConstruction: Cancel construction
YearsOfIncome icon@Text amt@Double: Treasury contains at least #{icon} #{plainNum amt} #{plural amt "year's" "years'"} worth of income
LibertyDesire icon@Text amt@Double: #{icon} Liberty desire is at least #{colourPc False amt}
GainLibertyDesire icon@Text amt@Double: Gain #{icon} #{colourPc False amt} liberty desire
ColonialParent: This country's colonial parent:
Always yn@Bool: #{ifThenElseT yn "{{icon|yes}} Always" "{{icon|no}} Never"}
CapitalCultureDominant: The capital's {{icon|culture}} culture is dominant in the country
NumUnions icon@Text amt@Double: Has at least #{icon} #{roundNum amt} personal #{plural amt "union" "unions"}
NumVassals icon@Text amt@Double: Has at least #{icon} #{roundNum amt} #{plural amt "vassal" "vassals"}
FreeVassal whom@Text: Release vassal #{whom} as an independent country
HasMissionary yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} an active missionary
NavyPercentage icon@Text amt@Double: Navy is at least #{plainPc amt} of #{icon} force limit
WasForceConverted yn@Bool: Was#{ifThenElseT yn "" " ''not''"} force converted
FemaleEmperorAllowed yn@Bool: The Emperor may#{ifThenElseT yn "" " ''not''"} be female
ImperialAuthority icon@Text amt@Double: #{icon} Imperial Authority is at least #{roundNum amt}
HasFemaleHeir yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} a female heir
Piety icon@Text amt@Double: Has at least #{icon} #{reducedNum (colourPcSign True) amt} piety
IsInTutorial yn@Bool: Is#{ifThenElseT yn "" " ''not''"} in the tutorial
GainFervor icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} fervor
GainChurchPower icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} church power
IncomeFromVassals icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Income from vassals
TimeToFabricateClaims icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Time to fabricate claims
DiploAnnexCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Diplomatic annexation cost
AEImpact icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Aggressive expansion impact
DiploRep icon@Text amt@Double: #{icon} #{colourNumSign True amt} Diplomatic reputation
EnvoyTravelTime icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Envoy travel time
DiploRelations icon@Text amt@Double: #{icon} #{colourNumSign True amt} Diplomatic relations
VassalForcelimitContribution icon@Text amt@Double: #{icon} #{colourNumSign True amt} Vassal force limit contribution
UnjustifiedDemands icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Unjustified demands
InfantryCombatAbility icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Infantry combat ability
CavalryCombatAbility icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Cavalry combat ability
ArtilleryCombatAbility icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Artillery combat ability
ShipDurability icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Ship durability
MoraleOfArmies icon@Text amt@Double: #{icon} #{reducedNum (colourNumSign True) amt} Morale of armies
MoraleOfNavies icon@Text amt@Double: #{icon} #{reducedNum (colourNumSign True) amt} Morale of navies
NavalAttrition icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Naval attrition
Discipline icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Discipline
NationalManpowerMod icon@Text amt@Double: #{icon} #{reducedNum (colourNumSign True) amt} National manpower modifier
ManpowerRecoverySpeed icon@Text amt@Double: #{icon} #{reducedNum (colourNumSign True) amt} Manpower recovery speed
AvailableMercs icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Available mercenaries
GarrisonSize icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Garrison size
LandAttrition icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Land attrition
LandForcelimitMod icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Land force limit modifier
PrestigeDecay icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Prestige decay
MercCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Mercenary cost
TechCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Technology cost
PossibleAdvisors icon@Text amt@Double: #{icon} #{reducedNum (colourNumSign True) amt} Possible advisors
ReduceInflationCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Reduce inflation cost
LeadersWithoutUpkeep icon@Text amt@Double: #{icon} #{reducedNum (colourNumSign True) amt} Leader(s)<!-- sic --> without upkeep
AdvisorCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Advisor costs
CoreCreationCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Core creation cost
MercMaintenance icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Mercenary maintenance modifier
ADMTechCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Administrative technology cost
DIPTechCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Diplomatic technology cost
GoodsProducedMod icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Goods produced modifier
NavalLeaderFire icon@Text amt@Double: #{icon} #{reducedNum (colourNumSign True) amt} Naval leader fire
NavalLeaderShock icon@Text amt@Double: #{icon} #{reducedNum (colourNumSign True) amt} Naval leader shock
PrestigeFromLand icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Prestige from land battles
PrestigeFromNaval icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Prestige from naval battles
Diplomats icon@Text amt@Double: #{icon} #{colourNumSign True amt} Diplomats
WarExhaustionCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Cost of reducing war exhaustion
ImproveRelations icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Improve relations
ProvinceWarscoreCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Province warscore cost
ReducedStabImpacts: Lowered impact on stability from diplomatic actions
NationalUnrest icon@Text amt@Double: #{icon} #{colourNumSign False amt} National unrest
YearsOfSeparatism icon@Text amt@Double: #{icon} #{colourNumSign False amt} Years of separatism
AccCultureThreshold icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Accepted culture threshold
BetterRelationsOverTime icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Better relations over time
IdeaCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Idea cost
Merchants icon@Text amt@Double: #{icon} #{colourNumSign True amt} Merchants
RecruitmentTime icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Recruitment time
ShipbuildingTime icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Shipbuilding time
GlobalTradePower icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Global trade power
EmbargoEff icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Embargo efficiency
PrivateerEff icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Privateer efficiency
GlobalSpyDefence icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} National spy defence
MaySabotageReputation: {{icon|may sabotage reputation|28px}} May [[sabotage reputation]]
SpyOffense icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Spy offense
CovertActionRelationImpact icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Covert action relation impact
MayStudyTech: {{icon|may study technology|28px}} May [[study technology]]
MaySowDiscontent: {{icon|may sow discontent|28px}} May [[sow discontent]]
MayAgitateForLiberty: May [[agitate for liberty]]
MayInfiltrateAdministration: {{icon|may infiltrate administration|28px}} May [[infiltrate administration]]
RebelSupportEff icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Rebel support efficiency
ArmyTraditionDecay icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Army tradition decay
NavyTraditionDecay icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Navy tradition decay
InfantryCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Infantry cost
CavalryCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Cavalry cost
MILTechCost icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Military technology cost
HostileCoreCreation icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Hostile core-creation cost on us
CaravanPower icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Caravan power
LandLeaderFire icon@Text amt@Double: #{icon} #{colourNumSign True amt} Land leader fire
LandLeaderShock icon@Text amt@Double: #{icon} #{colourNumSign True amt} Land leader shock
LandLeaderManeuver icon@Text amt@Double: #{icon} #{colourNumSign True amt} Land leader maneuver
LeaderSiege icon@Text amt@Double: #{icon} #{colourNumSign True amt} Leader siege
FortDefense icon@Text amt@Double: #{icon} #{colourPcSign True amt} Fort defense
FortMaintenance icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign False) amt} Fort maintenance
ReinforceSpeed icon@Text amt@Double: #{icon} #{colourPcSign True amt} Reinforce speed
AttritionForEnemies icon@Text amt@Double: #{icon} #{colourNumSign True amt} Attrition for enemies
SiegeAbility icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Siege ability
RecoverArmyMoraleSpeed icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Recover army morale speed
TradeRange icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Trade range
TradeSteering icon@Text amt@Double: #{icon} #{reducedNum (colourPcSign True) amt} Trade steering
