use anyhow::anyhow;
use enum_iterator::Sequence;
use std::{cmp::Ordering, collections::HashMap, iter::zip, str::FromStr};
use winnow::{
    ascii::{digit1, line_ending},
    combinator::{dispatch, fail, repeat, separated, separated_pair, success, terminated},
    prelude::*,
    token::any,
};

crate::aoc!();

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let camel_cards = input.parse::<CamelCards>()?;
    let solution = camel_cards.winnings();
    Ok(solution)
}

struct CamelCards {
    hand_and_bids: Vec<(Hand, usize)>,
}

impl CamelCards {
    fn winnings(&self) -> usize {
        let mut hand_and_bids = self.hand_and_bids.clone();
        hand_and_bids.sort_by(|(x, _), (y, _)| x.cmp(y));
        hand_and_bids
            .into_iter()
            .enumerate()
            .map(|(rank, (_hand, bid))| bid * (rank + 1))
            .sum()
    }
}

impl FromStr for CamelCards {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        camel_cards.parse(s).map_err(|e| anyhow!(e.to_string()))
    }
}

fn camel_cards(input: &mut &str) -> PResult<CamelCards> {
    let hand_and_bids = terminated(
        separated(1.., separated_pair(hand, " ", usize), line_ending),
        repeat::<_, _, (), _, _>(0.., line_ending),
    )
    .parse_next(input)?;
    Ok(CamelCards { hand_and_bids })
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct Hand {
    cards: [Card; 5],
}

impl Hand {
    fn to_map(self) -> HashMap<Card, usize> {
        let mut map = HashMap::with_capacity(13);

        for card in enum_iterator::all::<Card>() {
            map.insert(card, 0);
        }

        for card in self.cards {
            map.entry(card).and_modify(|count| *count += 1);
        }

        map
    }

    fn kind(self) -> Kind {
        if self.is_five_of_a_kind() {
            return Kind::FiveOfAKind;
        }

        if self.is_four_of_a_kind() {
            return Kind::FourOfAKind;
        }

        if self.is_full_house() {
            return Kind::FullHouse;
        }

        if self.is_three_of_a_kind() {
            return Kind::ThreeOfAKind;
        }

        if self.is_two_pair() {
            return Kind::TwoPair;
        }

        if self.is_one_pair() {
            return Kind::OnePair;
        }

        Kind::HighCard
    }

    fn is_five_of_a_kind(self) -> bool {
        self.to_map().into_values().any(|count| count == 5)
    }

    fn is_four_of_a_kind(self) -> bool {
        self.to_map().into_values().any(|count| count == 4)
    }

    fn is_full_house(self) -> bool {
        let mut three = false;
        let mut two = false;

        self.to_map().into_values().for_each(|count| {
            if count == 3 {
                three = true;
            } else if count == 2 {
                two = true;
            }
        });

        three && two
    }

    fn is_three_of_a_kind(self) -> bool {
        self.to_map().into_values().any(|count| count == 3)
    }

    fn is_two_pair(self) -> bool {
        let mut pairs = 0;

        self.to_map().into_values().for_each(|count| {
            if count == 2 {
                pairs += 1;
            }
        });

        pairs == 2
    }

    fn is_one_pair(self) -> bool {
        self.to_map().into_values().any(|count| count == 2)
    }

    #[allow(dead_code)]
    fn is_high_card() -> bool {
        true
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.kind().cmp(&other.kind()) {
            Ordering::Less => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
            Ordering::Equal => {
                for (self_card, other_card) in zip(self.cards, other.cards) {
                    match self_card.cmp(&other_card) {
                        Ordering::Less => return Ordering::Less,
                        Ordering::Greater => return Ordering::Greater,
                        Ordering::Equal => continue,
                    }
                }
                Ordering::Equal
            }
        }
    }
}

fn hand(input: &mut &str) -> PResult<Hand> {
    let cards: Vec<_> = repeat(5, card).parse_next(input)?;
    let cards = [cards[0], cards[1], cards[2], cards[3], cards[4]];
    Ok(Hand { cards })
}

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd, Sequence)]
enum Card {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

fn card(input: &mut &str) -> PResult<Card> {
    dispatch! {any;
        'A' => success(Card::Ace),
        'K' => success(Card::King),
        'Q' => success(Card::Queen),
        'J' => success(Card::Jack),
        'T' => success(Card::Ten),
        '9' => success(Card::Nine),
        '8' => success(Card::Eight),
        '7' => success(Card::Seven),
        '6' => success(Card::Six),
        '5' => success(Card::Five),
        '4' => success(Card::Four),
        '3' => success(Card::Three),
        '2' => success(Card::Two),
        _ => fail,
    }
    .parse_next(input)
}

#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
#[allow(clippy::enum_variant_names)]
enum Kind {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

fn usize(input: &mut &str) -> PResult<usize> {
    digit1.try_map(|s: &str| s.parse()).parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    const SAMPLE: &str = indoc! {"
        32T3K 765
        T55J5 684
        KK677 28
        KTJJT 220
        QQQJA 483
    "};

    #[test]
    fn test_sample() {
        assert_eq!(solve(SAMPLE).unwrap(), 6440);
    }

    #[test]
    fn test_ord() {
        assert!(Card::Ace > Card::Two);
        assert!(Card::Ace > Card::King);
        assert!(Kind::FiveOfAKind > Kind::FourOfAKind);
        assert!(Kind::FiveOfAKind > Kind::HighCard);
    }
}
