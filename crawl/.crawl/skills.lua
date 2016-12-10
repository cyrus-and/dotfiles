{

local SKILL_NAMES = {
    'Fighting',
    'Short Blades',
    'Long Blades',
    'Axes',
    'Maces & Flails',
    'Polearms',
    'Staves',
    'Slings',
    'Bows',
    'Crossbows',
    'Throwing',
    'Armour',
    'Dodging',
    'Stealth',
    'Shields',
    'Unarmed Combat',
    'Spellcasting',
    'Conjurations',
    'Hexes',
    'Charms',
    'Summonings',
    'Necromancy',
    'Translocations',
    'Transmutations',
    'Fire Magic',
    'Ice Magic',
    'Air Magic',
    'Earth Magic',
    'Poison Magic',
    'Invocations',
    'Evocations'}

local function reset()
    for _, skill in pairs(SKILL_NAMES) do
        you.train_skill(skill, 0)
    end
end

-- TODO add dependency SP > 10: inv cappet to 5

function profile(race, class, skills)
    if you.race() == race and you.class() == class then
        reset()
        for skill, directive in pairs(skills) do
            if you.can_train_skill(skill) and
               (not directive.cap or you.skill(skill) < directive.cap) then
                you.train_skill(skill, directive.focus and 2 or 1)
            else
                you.train_skill(skill, 0)
            end
        end
    end
end

}
