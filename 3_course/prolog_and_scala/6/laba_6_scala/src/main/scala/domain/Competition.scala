package domain

// Case class для представления соревнований
case class Competition(rank: String, sport: String, year: Int, country: String, teams: List[Team])