import domain.{Competition, Result, Team}

import scala.annotation.tailrec

/*
  Предметная область – спортивные соревнования.
  соревнование может быть описано структурой: ранг соревнований, вид спорта,
  год проведения, страна проведения, список команд - участников. Команды -
  участники могут быть описаны следующей структурой: название команды,
  страна, результаты соревнований.
  Результаты соревнований могут быть описаны списком структур: название команды – соперника,
  страна, тип результата (выигрыш, проигрыш, ничья).
*/

object main extends App {
  // Коллекция соревнований
  val competitions = List(
    Competition("Олимпийские игры", "Баскетбол", 2016, "Бразилия", List(
      Team("США", "США", List(
        Result("Испания", "Испания", "Победа"),
        Result("Аргентина", "Аргентина", "Победа"),
        Result("Франция", "Франция", "Победа"),
      )),
      Team("Испания", "Испания", List(
        Result("США", "США", "Поражение"),
        Result("Аргентина", "Аргентина", "Победа"),
        Result("Франция", "Франция", "Ничья"),
      )),
      Team("Аргентина", "Аргентина", List(
        Result("США", "США", "Поражение"),
        Result("Испания", "Испания", "Поражение"),
        Result("Франция", "Франция", "Победа"),
      )),
    )),
    Competition("Олимпийские игры", "Лыжный спорт", 2020, "Бразилия", List(
      Team("США", "США", List(
        Result("Испания", "Испания", "Победа"),
        Result("Аргентина", "Аргентина", "Победа"),
        Result("Франция", "Франция", "Победа"),
      )),
      Team("Испания", "Испания", List(
        Result("США", "США", "Поражение"),
        Result("Аргентина", "Аргентина", "Победа"),
        Result("Франция", "Франция", "Ничья"),
      )),
      Team("Аргентина", "Аргентина", List(
        Result("США", "США", "Поражение"),
        Result("Испания", "Испания", "Поражение"),
        Result("Франция", "Франция", "Победа"),
      )),
    )),
    Competition("Чемпионат мира", "Футбол", 2022, "Катар", List(
      Team("Аргентина", "Аргентина", List(
        Result("Саудовская Аравия", "Саудовская Аравия", "Победа"),
        Result("Мексика", "Мексика", "Ничья"),
        Result("Польша", "Польша", "Победа"),
      )),
      Team("Бразилия", "Бразилия", List(
        Result("Сербия", "Сербия", "Победа"),
        Result("Швейцария", "Швейцария", "Победа"),
        Result("Камерун", "Камерун", "Ничья"),
      )),
    )),
  )

  // Функция для поиска годов, в которые участвовало максимальное число команд в заданном ранге соревнований
  def findMaxTeamsYears(rank: String, competitions: List[Competition]): List[Int] = {
    // Функция для поиска максимального числа команд в заданном ранге соревнований
    def findMaxTeams(competitions: List[Competition]): Int = competitions match {
      case Nil => 0
      case head :: tail =>
        if (head.rank == rank) math.max(head.teams.size, findMaxTeams(tail))
        else findMaxTeams(tail)
    }

    // Максимальное число команд
    val maxTeams = findMaxTeams(competitions)

    // Функция для поиска годов, когда участвовало максимальное число команд
    @tailrec
    def helper(maxTeams: Int, competitions: List[Competition], result: List[Int]): List[Int] = competitions match {
      case Nil => result
      case head :: tail =>
        if (head.rank == rank && head.teams.size == maxTeams) helper(maxTeams, tail, head.year :: result)
        else helper(maxTeams, tail, result)
    }
    helper(maxTeams, competitions, Nil).distinct
  }

  // Найти вид спорта, в котором выступает заданная команда;
  def findSportsByTeam(teamName: String, competitions: List[Competition]): List[String] = {
    @tailrec
    def helper(competitions: List[Competition], acc: List[String]): List[String] = competitions match {
      case Nil => acc
      case head :: tail => head.teams.find(_.name == teamName) match {
        case Some(_) => helper(tail, head.sport :: acc)
        case None => helper(tail, acc)
      }
    }
    helper(competitions, List.empty).distinct // Инициализируем накапливаемый результат пустым списком
  }

  // Функция для поиска всех команд, которые участвовали в Олимпийских играх по определенному виду спорта
  def findOlympicTeams(sport: String, competitions: List[Competition]): List[String] = {
    @tailrec
    def helper(result: List[String], competitions: List[Competition]): List[String] = competitions match {
      case Nil => result
      case head :: tail =>
        if (head.rank == "Олимпийские игры" && head.sport == sport) helper(result ++ head.teams.map(_.name), tail)
        else helper(result, tail)
    }

    helper(List.empty, competitions).distinct
  }

  // Функция для поиска всех команд, участвовавших в соревнованиях в заданном году
  def findTeamsByYear(year: Int, competitions: List[Competition]): List[String] = {
    @tailrec
    def helper(result: List[String], competitions: List[Competition]): List[String] = competitions match {
      case Nil => result
      case head :: tail =>
        if (head.year == year) helper(result ++ head.teams.map(_.name), tail)
        else helper(result, tail)
    }

    helper(List.empty, competitions).distinct
  }

  // Функция для поиска всех команд определенной страны, у которых были выигрыши
  def findWinningTeamsByCountry(country: String, competitions: List[Competition]): List[String] = {

    @tailrec
    def helper(result: List[String], competitions: List[Competition]): List[String] = competitions match {
      case Nil => result
      case head :: tail =>
        val winningTeams = head.teams.filter(_.country == country).filter(_.results.exists(_.outcome == "Победа"))
        helper(result ++ winningTeams.map(_.name), tail) // Хвостовой вызов
    }

    helper(List.empty, competitions).distinct
  }

  // Тесты
  println(findMaxTeamsYears("Олимпийские игры", competitions))
  println(findSportsByTeam("Аргентина", competitions))
  println(findOlympicTeams("Баскетбол", competitions))
  println(findTeamsByYear(2022, competitions))
  println(findWinningTeamsByCountry("США", competitions))
}
