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
GainInflation icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum False amt} inflation
GainLocalAutonomy icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum False amt} local autonomy
ReformDesire icon@Text amt@Double: {{icon|catholic}} Catholicism has at least #{icon} #{reducedNum plainPc amt} reform desire
GainReformDesire icon@Text amt@Double: {{icon|catholic}} Catholicism #{gainsOrLoses amt} #{icon} #{reducedNum (colourPc False) amt} reform desire
GainMercantilism icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} mercantilism
GainMP icon@Text amt@Double: #{gainOrLose amt} #{icon} #{colourNum True amt} manpower
GainMPFrac icon@Text amt@Double: #{gainOrLose amt} #{icon} manpower equal to #{colourPc True amt} of maximum
CountryMod: country modifier
ProvMod: province modifier
PermanentProvMod: permanent province modifier
RulerMod: ruler modifier
TradeMod: trade modifier
GainMod type@Text name@Text: Gain #{type} #{iquotes name}
GainModDur type@Text name@Text days@Double: Gain #{type} #{iquotes name} for #{roundNumNoSpace days} #{plural days "day" "days"}
GainModPow type@Text name@Text pow@Double: Gain #{type} #{iquotes name} (#{colourNum True pow} Power)
GainModPowDur type@Text name@Text pow@Double days@Double: Gain #{type} #{iquotes name} (#{colourNum True pow} Power) for #{plainNum days} #{plural days "day" "days"}
ActorGainsMod who@Text type@Text name@Text: #{who} gains #{type} #{iquotes name}
ActorGainsModDur who@Text type@Text name@Text days@Double: #{who} gains #{type} #{iquotes name} for #{plainNum days} #{plural days "day" "days"}
ActorGainsModPow who@Text type@Text name@Text pow@Double: #{who} gains #{type} #{iquotes name} (#{colourNum True pow} Power)
ActorGainsModPowDur who@Text type@Text name@Text pow@Double days@Double: #{who} gains #{type} #{iquotes name} (#{colourNum True pow} Power) for #{plainNum days} #{plural days "day" "days"}
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
AnyKnownCountry: Any known country:
AnyNeighborCountry: Any neighbouring country:
AnyNeighborProvince: Any neighbouring province:
AnyOwnedProvince: Any owned province:
AnyRival: Any rival:
Capital: Capital:
Controller: Province controller:
Emperor: The Holy Roman Emperor:
EveryCountry: Every country in the world:
EveryEnemyCountry: Every enemy country:
EveryKnownCountry: Every known country:
EveryNeighborCountry: Every neighbouring country:
EveryNeighborProvince: Every neighbouring province:
EveryOwnedProvince: Every owned province:
EveryProvince: Every province in the world:
EverySubject: Every subject country:
HiddenEffect: Hidden effect:
If: If:
Limit: Limited to:
Owner: Province owner:
RandomAlly: One random ally:
RandomCoreCountry: One random country with a core on this province:
RandomCountry: One random country:
RandomList: One of the following at random:
RandomNeighborCountry: One random neighbouring country:
RandomNeighborProvince: One random neighbouring province:
RandomOwnedProvince: One random owned province:
RandomProvince: One random province:
RandomChance chance@Double: #{plainPc chance} chance of:
Random: At random:
ChangeGovernment what@Text: Change government to #{what}
ContinentIs what@Text: Continent is #{what}
CultureIs what@Text: Culture is #{what}
CultureIsGroup what@Text: Culture is in #{what} group
RulerIsDynasty what@Text: Ruler is of #{what} dynasty
DisasterEnds what@Text: Disaster #{iquotes what} ends
GovernmentIs what@Text: Government is #{what}
HasAdvisor who@Text: Has advisor #{who}
HasAdvisorType icon@Text what@Text: Has #{icon} #{what} advisor
HasTerrain what@Text: Has #{what} terrain
InfantrySpawns what@Text: An infantry regiment spawns in #{what}
AdvisorDies who@Text: Advisor #{who} dies
PrimaryCultureIs what@Text: Primary culture is #{what}
RegionIs what@Text: Province is in #{what} region
LoseAdvisor who@Text: Advisor #{who} leaves the country's court
RemoveFromEstate icon@Text who@Text: Remove province from the #{icon} #{who} estate
DisasterOngoing what@Text: The #{iquotes what} disaster is ongoing
ProvinceIs what@Text: Province is #{what}
Owns what@Text: Owns province #{what}
OwnsCore what@Text: Owns core province #{what}
AdvisorExists advisorID@Double: Advisor with ID #{plainNum advisorID} exists
AdvisorIsEmployed advisorID@Double: Advisor with ID #{plainNum advisorID} is employed
ClearFlag flagType@Text name@Text: Clear #{flagType} flag <tt>#{name}</tt>
HasFlag flagType@Text name@Text: #{T.toTitle flagType} flag <tt>#{name}</tt> is set
SetFlag flagType@Text name@Text: Set #{flagType} flag <tt>#{name}</tt>
HadFlag category@Text name@Text days@Double:
CountryFlag: country
ProvinceFlag: province
RulerFlag: ruler
GlobalFlag: global
HadCountryFlag _icon@Text name@Text days@Double: Has had country flag <tt>#{name}</tt> for at least #{roundNum days} days
HadProvinceFlag _icon@Text name@Text days@Double: Has had province flag <tt>#{name}</tt> for at least #{roundNum days} days
HadRulerFlag _icon@Text name@Text days@Double: Has had ruler flag <tt>#{name}</tt> for at least #{roundNum days} days
HadGlobalFlag _icon@Text name@Text days@Double: Global flag <tt>#{name}</tt> has been set for at least #{roundNum days} days
ColonySettlers amt@Double: Colony has at least #{roundNum amt} settlers
WasAtWar amt@Double: Was at war within the last #{roundNum amt} months
HeirAge amt@Double: Heir is at least #{roundNum amt} #{plural amt "year" "years"} old
YearIs amt@Double: Year is #{roundNumNoSpace amt} or later
NumLoans amt@Double: Has at least #{plainNum amt} loans
NumMercs amt@Double: Has at least #{roundNum amt} mercenary #{plural amt "regiment" "regiments"}
NumPorts amt@Double: Has at least #{plainNum amt} #{plural amt "port" "ports"}
NumRebelArmies amt@Double: At least #{roundNum amt} rebel #{plural (round amt) "army is" "armies are"} present in the country
NumEmbargoes amt@Double: Is embargoing at least #{roundNum amt} other #{plural (round amt) "country" "countries"}
UnitsInProvince amt@Double: Province contains at least #{roundNum amt} #{plural (round amt) "regiment" "regiments"}
NumCities amt@Double: Owns at least #{roundNum amt} non-colony #{plural amt "province" "provinces"}
NumCitiesThan who@Text: Owns at least as many cities as #{who}
ToleranceToThis amt@Double: Tolerance to this religion is at least #{colourNum True amt}
RulerADM icon@Text amt@Double: Ruler's #{icon} administrative skill is at least #{roundNum amt}
ADMTech icon@Text amt@Double: #{icon} Administrative technology is at least #{roundNum amt}
ArmyTradition icon@Text amt@Double: #{icon} Army tradition is at least #{plainNum amt}
BaseManpower icon@Text amt@Double: #{icon} Base manpower is at least #{roundNum amt}
BaseProduction icon@Text amt@Double: #{icon} Base production is at least #{roundNum amt}
BaseTax icon@Text amt@Double: #{icon} Base tax is at least #{roundNum amt}
CreateAdmiral icon@Text amt@Double: Gain #{icon} admiral with #{roundNum amt} navy tradition
CreateGeneral icon@Text amt@Double: Gain #{icon} general with #{roundNum amt} army tradition
Development icon@Text development@Double: #{icon} Development is at least #{roundNum development}
RulerDIP icon@Text amt@Double: Ruler's #{icon} diplomatic skill is at least #{roundNum amt}
DIPTech icon@Text amt@Double: #{icon} Diplomatic technology is at least #{roundNum amt}
HordeUnity icon@Text amt@Double: #{icon} Horde unity is at least #{roundNum amt}
Karma icon@Text amt@Double: #{icon} Karma is at least #{colourPc True amt}
Legitimacy icon@Text amt@Double: #{icon} Legitimacy is at least #{roundPc amt}
RulerMIL icon@Text amt@Double: Ruler's #{icon} military skill is at least #{roundNum amt}
MILTech icon@Text amt@Double: #{icon} Military technology is at least #{roundNum amt}
NumAllies icon@Text amt@Double: Has at least #{icon} #{roundNum amt} #{plural amt "ally" "allies"}
NumCardinals icon@Text amt@Double: Has at least #{icon} #{roundNum amt} #{plural amt "cardinal" "cardinals"}
NumColonists icon@Text amt@Double: Has at least #{icon} #{roundNum amt} #{plural amt "colonist" "colonists"}
Stability icon@Text amt@Double: Stability is at least #{icon} #{colourNum True amt}
TotalDevelopment icon@Text amt@Double: Country's total #{icon} development is at least #{roundNum amt}
TotalCardinals icon@Text amt@Double: At least #{icon} #{roundNum amt} #{plural amt "cardinal" "cardinals"} exist in the world
Unrest icon@Text amt@Double: Unrest is at least #{icon} #{colourNum False amt}
MonthlyIncome icon@Text amt@Double: Monthly income is at least #{icon} #{colourNum True amt} ducats
WarExhaustion icon@Text amt@Double: War exhaustion is at least #{icon} #{colourNum False amt}
WarScore icon@Text amt@Double: War score is at least #{icon} #{colourPc True amt}
RepTrad icon@Text amt@Double: #{icon} Republican tradition is at least #{reducedNum roundPc amt}
Inflation icon@Text amt@Double: #{icon} Inflation is at least #{colourPc False amt}
LocalAutonomy icon@Text amt@Double: #{icon} Local autonomy is at least #{colourPc False amt}
ManpowerPercentage icon@Text amt@Double: #{icon} Manpower reserves are at least #{reducedNum plainPc amt} of maximum
Mercantilism icon@Text amt@Double: #{icon} Mercantilism is at least #{reducedNum plainPc amt}
ChangeGoods icon@Text what@Text: Change province's goods produced to #{icon} #{what}
CreateAdvisor icon@Text what@Text: Gain #{icon} #{what} advisor
HasIdeaGroup icon@Text what@Text: Has activated the #{icon} #{what} idea group
ProducesGoods icon@Text what@Text: Produces #{icon} #{what}
HasEstate icon@Text what@Text: Has estate #{icon} #{what}
AssignToEstate icon@Text what@Text: Assign province to the #{icon} #{what} estate
RulerIsGeneral icon@Text what@Text: Ruler is a #{icon} #{what} general
AlliedWith who@Text: Is allied to #{who}
CedeProvinceTo who@Text: Cede province to #{who}
ControlledBy who@Text: Province is controlled by #{who}
DefensiveWarAgainst who@Text: Is defending in a war against #{who}
DiscoverCountry who@Text: Discover #{who}
GainClaim who@Text: #{who} gains a claim on this province
GainPermanentClaim who@Text: #{who} gains a permanent claim on this province
HasDiscovered who@Text: Has discovered #{who}
Inherit who@Text: Inherit #{who}
Neighbors who@Text: Has a border with #{who}
IsSubjectOf who@Text: Is a subject of #{who}
LoseCore who@Text: #{who} loses their core on this province
RoyalMarriageWith who@Text: Has a royal marriage with #{who}
OffensiveWarAgainst who@Text: Is attacking in a war against #{who}
OwnedBy who@Text: Province is owned by #{who}
ReleaseVassal who@Text: Release #{who} as a vassal
UnderSiegeBy who@Text: Province is under siege by #{who}
CountryIs who@Text: Is #{who}
TruceWith who@Text: Has a truce with #{who}
AtWarWith who@Text: Is at war with #{who}
MakeWhitePeace who@Text: Make a white peace with #{who}
CountryExists who@Text: #{who} exists
SameReligion who@Text: Religion is same as #{who}
Religion icon@Text what@Text: Religion is #{icon} #{what}
SameReligionGroup who@Text: Religion group is same as #{who}
ReligionGroup _@Text what@Text: Religion is in #{what} group
ChangeSameReligion who@Text: Change religion to that of #{who}
ChangeReligion icon@Text what@Text: Change religion to #{icon} #{what}
IsCoreOf who@Text: Is a core of #{who}
HasCoreOn what@Text: Has a core on #{what}
HasClaim who@Text: #{who} has a claim on this province
HasClaimOn what@Text: Has a claim on #{what}
IsAIControlled yn@Bool: Is #{ifThenElseT yn "AI" "player"}-controlled
HasCardinal yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} a cardinal
HasHeir yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} an heir
HasOwnerReligion yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} the same religion as its owner
HasPort yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} a port
HasSeatInParliament yn@Bool: #{ifThenElseT yn "Has" "Does ''not'' have"} a seat in Parliament
IsInRegency yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} in a regency
UnderSiege yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} under siege
AtWar yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} at war
IsCapital yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} the country's capital
IsCity yn@Bool: #{ifThenElseT yn "Is a city" "Is a colony"}
IsEmperor yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} the Holy Roman Emperor
IsFemale yn@Bool: Is #{ifThenElseT yn "female" "male"}
IsLesserInUnion yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} a lesser partner in a personal union
IsLooted yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} looted
IsOverseas yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} overseas
IsPartOfHRE yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} part of the Holy Roman Empire
IsCenterOfReformation yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} a Center of Reformation
IsSubject yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} a subject nation
PapacyIsActive yn@Bool: Papal interaction #{ifThenElseT yn "is" "is ''not''"} active
HasBeenPlayer yn@Bool: #{ifThenElseT yn "Has" "Has ''never''"} been player-controlled
GainCB cbtype@Text who@Text: Gain #{cbtype} casus belli against #{who}
GainCBDuration cbtype@Text who@Text months@Double: Gain #{cbtype} casus belli against #{who} for #{roundNum months} months
ReverseGainCB cbtype@Text who@Text: #{who} gains #{cbtype} casus belli against this country
ReverseGainCBDuration cbtype@Text who@Text months@Double: #{who} gains #{cbtype} casus belli against this country for #{roundNum months} months
FactionGainInfluence icon@Text who@Text amt@Double: #{gainOrLose amt} #{icon} #{plainNum amt} influence for the #{who} faction
HasBuilding icon@Text what@Text: Has #{icon} #{what} building
Indefinitely: indefinitely
ForDays days@Double: for #{roundNum days} days
EstateLoyalty icon@Text who@Text amt@Double: #{icon} #{who} has at least #{plainNum amt} loyalty
EstateInfluence icon@Text who@Text amt@Double: #{icon} #{who} has at least #{plainNum amt} influence
AddEstateLoyalty icon@Text who@Text amt@Double: #{icon} #{who} #{gainsOrLoses amt} #{plainNum amt} loyalty
AddEstateInfluence icon@Text who@Text amt@Double: #{icon} #{who} #{gainsOrLoses amt} #{plainNum amt} influence
EstateLoyaltyModifier icon@Text who@Text what@Text amt@Double dur@Text: #{icon} #{who} gains loyalty modifier #{iquotes what} (#{colourNum True amt} loyalty) #{dur}
EstateInfluenceModifier icon@Text who@Text what@Text amt@Double dur@Text: #{icon} #{who} gains influence modifier #{iquotes what} (#{colourNum True amt} influence) #{dur}
AddOpinion what@Text who@Text: Gain opinion modifier #{iquotes what} towards #{who}
ReverseAddOpinion what@Text who@Text: #{who} gains opinion modifier #{iquotes what} towards this country
AddOpinionDur what@Text who@Text years@Double: Gain opinion modifier #{iquotes what} towards #{who} for #{plainNum years} years
ReverseAddOpinionDur what@Text who@Text years@Double: #{who} gains opinion modifier #{iquotes what} towards this country for #{plainNum years} years
HasOpinionMod what@Text who@Text: Has opinion modifier #{iquotes what} towards #{who}
RemoveOpinionMod what@Text who@Text: Lose opinion modifier #{iquotes what} towards #{who}
AddYearsOfIncome icon@Text amt@Double: Add #{icon} ducats equal to #{plainNum amt} #{plural amt "year" "years"} of income
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
DeclareWarWithCB who@Text cb@Text: Declare war on #{who} using #{cb} casus belli
GainAdvisor skill@Double: Gain skill #{roundNum skill} advisor
GainAdvisorDiscount skill@Double: Gain skill #{roundNum skill} advisor (50% cheaper to employ)
GainAdvisorLoc where@Text skill@Double: Gain skill #{roundNum skill} advisor in #{where}
GainAdvisorLocDiscount where@Text skill@Double: Gain skill #{roundNum skill} advisor in #{where} (50% cheaper to employ)
GainAdvisorName name@Text skill@Double: Gain skill #{roundNum skill} advisor named #{name}
GainAdvisorNameDiscount name@Text skill@Double: Gain skill #{roundNum skill} advisor named #{name} (50% cheaper to employ)
GainAdvisorNameLoc name@Text where@Text skill@Double: Gain skill #{roundNum skill} advisor named #{name} in #{where}
GainAdvisorNameLocDiscount name@Text where@Text skill@Double: Gain skill #{roundNum skill} advisor named #{name} in #{where} (50% cheaper to employ)
GainAdvisorType advtype@Text skill@Double: Gain skill #{roundNum skill} #{advtype}
GainAdvisorTypeDiscount advtype@Text skill@Double: Gain skill #{roundNum skill} #{advtype} (50% cheaper to employ)
GainAdvisorTypeLoc advtype@Text where@Text skill@Double: Gain skill #{roundNum skill} #{advtype} in #{where}
GainAdvisorTypeLocDiscount advtype@Text where@Text skill@Double: Gain skill #{roundNum skill} #{advtype} in #{where} (50% cheaper to employ)
GainAdvisorTypeName advtype@Text name@Text skill@Double: Gain skill #{roundNum skill} #{advtype} named #{name}
GainAdvisorTypeNameDiscount advtype@Text name@Text skill@Double: Gain skill #{roundNum skill} #{advtype} named #{name} (50% cheaper to employ)
GainAdvisorTypeNameLoc advtype@Text name@Text where@Text skill@Double: Gain skill #{roundNum skill} #{advtype} named #{name} in #{where}
GainAdvisorTypeNameLocDiscount advtype@Text name@Text where@Text skill@Double: Gain skill #{roundNum skill} #{advtype} named #{name} in #{where} (50% cheaper to employ)
GainFemaleAdvisor female@Bool skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor
GainFemaleAdvisorDiscount female@Bool skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor (50% cheaper to employ)
GainFemaleAdvisorLoc female@Bool where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor in #{where}
GainFemaleAdvisorLocDiscount female@Bool where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor in #{where} (50% cheaper to employ)
GainFemaleAdvisorName female@Bool name@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor named #{name}
GainFemaleAdvisorNameDiscount female@Bool name@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor named #{name} (50% cheaper to employ)
GainFemaleAdvisorNameLoc female@Bool name@Text where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor named #{name} in #{where}
GainFemaleAdvisorNameLocDiscount female@Bool name@Text where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} advisor named #{name} in #{where} (50% cheaper to employ)
GainFemaleAdvisorType female@Bool advtype@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype}
GainFemaleAdvisorTypeDiscount female@Bool advtype@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} (50% cheaper to employ)
GainFemaleAdvisorTypeLoc female@Bool advtype@Text where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} in #{where}
GainFemaleAdvisorTypeLocDiscount female@Bool advtype@Text where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} in #{where} (50% cheaper to employ)
GainFemaleAdvisorTypeName female@Bool advtype@Text name@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} named #{name}
GainFemaleAdvisorTypeNameDiscount female@Bool advtype@Text name@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} named #{name} (50% cheaper to employ)
GainFemaleAdvisorTypeNameLoc female@Bool advtype@Text name@Text where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} named #{name} in #{where}
GainFemaleAdvisorTypeNameLocDiscount female@Bool advtype@Text name@Text where@Text skill@Double: Gain skill #{roundNum skill} #{ifThenElseT female "female" "male"} #{advtype} named #{name} in #{where} (50% cheaper to employ)
RebelLeaderRuler: The leader of the rebels becomes the country's new ruler
NewRuler regent@Bool: A new #{ifThenElseT regent "regent" "ruler"} comes to power
NewRulerAttribs regent@Bool: A new #{ifThenElseT regent "regent" "ruler"} comes to power with the following attributes:
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
StrongestTradePower who@Text: Strongest trade power in this node is #{who}
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
IsElector yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} an Elector of the Holy Roman Empire
NoHREReforms: No reforms have been passed in the Holy Roman Empire
HREPassedReform what@Text: The imperial reform ''#{what}'' has been passed
EnableHRELeagues: Leagues for the religion of the Holy Roman Empire begin to form.
IsInLeagueWar yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} involved in a religious league war
IsLeagueEnemy who@Text: Is in the opposite religious league to #{who}
ReligionYears icon@Text name@Text years@Double: The #{icon} #{name} religion has existed for at least #{plainNum years} years
HasIdea what@Text: Has idea ''#{what}''
ReligionProvinces icon@Text name@Text amt@Double: Country has at least #{roundNum amt} #{plural (round amt) "province" "provinces"} of #{icon} #{name} religion
GoodsProvinces icon@Text name@Text amt@Double: Country has at least #{roundNum amt} #{plural (round amt) "province" "provinces"} producing #{icon} #{name}
HasInnovativenessIdea name@Text num@Int: Has {{icon|innovative ideas}} Innovative idea #{show num} ''#{name}''
GainColonists icon@Text amt@Double: #{icon} #{colourNum True amt} Colonists
MayExplore: Can recruit explorers and conquistadors. Explorers may explore ocean provinces.
GainColonialRange icon@Text amt@Double: #{icon} #{colourNum True amt} Colonial range
GainGlobalSettlers icon@Text amt@Double: #{icon} #{colourNum True amt} Global settler increase
GainGlobalTariffs icon@Text amt@Double: #{icon} #{reducedNum (colourPc True) amt} Global tariffs
GainNavalForcelimitMod icon@Text amt@Double: #{icon} #{reducedNum (colourPc True) amt} Naval forcelimit modifier
GainPrimitivesCB: Gain permanent "Colonial Conquest" Casus Belli against all primitives.
GainYearlyNavyTradition icon@Text amt@Double: #{icon} #{colourNum True amt} Yearly naval tradition
GainLightShipPower icon@Text amt@Double: #{icon} #{reducedNum (colourPc True) amt} Light ship combat ability
GainGlobalShipRepair icon@Text amt@Double: #{icon} #{reducedNum (colourPc True) amt} Global ship repair
GainGlobalShipCost icon@Text amt@Double: #{icon} #{reducedNum (colourPc False) amt} Global ship cost
GainNavalLeaderManeuver icon@Text amt@Double: #{icon} #{colourNum True amt} Naval leader maneuver
GainBlockadeEfficiency icon@Text amt@Double: #{icon} #{reducedNum (colourPc True) amt} Blockade efficiency
GainSeaRepair: Ships repair while in coastal sea provinces.
Primitives yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} primitive
GainGlobalTaxModifier icon@Text amt@Double: #{icon} #{reducedNum (colourPc True) amt} Global tax modifier
GainBuildCost icon@Text amt@Double: #{icon} #{reducedNum (colourPc False) amt} Build cost
GainYearlyInflationReduction icon@Text amt@Double: #{icon} #{colourNum False (negate amt)} Yearly inflation reduction
GainInterestPerAnnum icon@Text amt@Double: #{icon} #{colourNum False amt} Interest per annum
GainGlobalAutonomy icon@Text amt@Double: #{icon} #{colourNum False amt} Monthly autonomy change
GainLandMaintenanceMod icon@Text amt@Double: #{icon} #{reducedNum (colourPc False) amt} Land maintenance modifier
GainProdEff icon@Text amt@Double: #{icon} #{reducedNum (colourPc True) amt} Production efficiency
GainDevelCost icon@Text amt@Double: #{icon} #{reducedNum (colourPc False) amt} {{DLC-only|Development cost}}
GainReligiousCB: Gain permanent "Holy War" and "Purging of Heresy" Casus Belli against heathens and heretics respectively.
GainMissionaries icon@Text amt@Double: #{icon} #{colourNum True amt} Missionaries
GainStabilityCost icon@Text amt@Double: #{icon} #{reducedNum (colourPc False) amt} Stability cost modifier
GainMissionaryStrength icon@Text amt@Double: #{icon} #{reducedNum (colourPc True) amt} Missionary strength
GainToleranceTrue icon@Text amt@Double: #{icon} #{colourNum True amt} Tolerance of the true faith
GainYearlyPapalInfluence icon@Text amt@Double: #{icon} #{colourNum True amt} Yearly papal influence
GainYearlyDevotion icon@Text amt@Double: #{icon} #{colourNum True amt} {{DLC-only|Yearly devotion}}
GainMonthlyFervor icon@Text amt@Double: #{icon} #{colourNum True amt} {{DLC-only|Monthly fervor}}
GainChurchPowerModifier icon@Text amt@Double: #{icon} #{colourNum True amt} {{DLC-only|Church power}}
GainYearlyPrestige icon@Text amt@Double: #{icon} #{colourNum True amt} Yearly prestige
GainMissionaryStrengthVsHeretics icon@Text amt@Double: #{icon} #{reducedNum (colourPc True) amt} Missionary strength vs heretics
GainCultureConvCost icon@Text amt@Double: #{icon} #{reducedNum (colourPc False) amt} Culture conversion cost
HasOpinion amt@Double who@Text: Has at least #{colourNum True amt} opinion of #{who}
NormalOrHistoricalNations yn@Bool: #{ifThenElseT yn "Playing" "''Not'' playing"} with normal or historical nations
IsCustomNation yn@Bool: #{ifThenElseT yn "Is" "Is ''not''"} playing a custom nation
ReligionEnabled icon@Text what@Text: The #{icon} #{what} religion is enabled
CapitalIs what@Text: Capital is #{what}
FullIdeaGroup icon@Text what@Text: Has completed #{icon} #{what}
TradeIncomePercentage amt@Double: Trade accounts for at least #{reducedNum plainPc amt} of national income
ReligiousUnity icon@Text amt@Double: #{icon} Religious unity is at least #{reducedNum (colourPc True) amt}
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
