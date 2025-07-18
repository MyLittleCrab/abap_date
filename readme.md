# ZCL_DATE - Advanced Date Manipulation Class for ABAP

[🇷🇺 Русский](#русский) | [🇺🇸 English](#english)

---

## English

### Overview
`ZCL_DATE` is a powerful ABAP class designed to simplify date manipulation and formatting operations. It provides an object-oriented approach to working with dates, offering convenient methods for date arithmetic, formatting, and localization.

### Features
- 🗓️ **Date Arithmetic**: Add/subtract days, months, and years
- 🌍 **Multi-language Support**: Localized month names
- 📅 **Flexible Formatting**: Customizable date string output
- 🔧 **Easy Initialization**: Multiple constructor options
- 📊 **Date Comparison**: Calculate differences between dates
- 🎯 **Type Safety**: Strong typing with proper ABAP types

### Installation
1. Import the class file `zcl_date.clas.abap` into your SAP system
2. Activate the class
3. The class is ready to use!

### Quick Start

#### Basic Initialization
```abap
DATA: lo_date TYPE REF TO zcl_date.

" Initialize with specific date
lo_date = zcl_date=>create( '20240101' ). " January 1, 2024

" Initialize with current date
lo_date = zcl_date=>create( ).

" Initialize with custom language
lo_date = zcl_date=>create( 
  iv_date = '20240101'
  iv_language = 'E'  " English
).
```

### API Reference

#### Getters
| Method | Return Type | Description |
|--------|-------------|-------------|
| `get_day()` | `num2` | Returns day of month (01-31) |
| `get_month_number()` | `num2` | Returns month number (01-12) |
| `get_year()` | `num4` | Returns full year (e.g., 2024) |
| `get_short_year()` | `num2` | Returns short year (e.g., 24) |
| `get_date()` | `dats` | Returns complete date |
| `get_month_name()` | `fcltx` | Returns full month name in current language |
| `get_short_month_name()` | `fcktx` | Returns abbreviated month name |
| `get_language()` | `lang` | Returns current language setting |

#### Setters
| Method | Parameter | Description |
|--------|-----------|-------------|
| `set_date()` | `dats` | Sets complete date |
| `set_day()` | `num2` | Sets day component |
| `set_month_number()` | `num2` | Sets month component |
| `set_year()` | `num4` | Sets year component |
| `set_language()` | `lang` | Sets language for formatting |

#### Date Arithmetic
```abap
" Date arithmetic examples
lo_date = zcl_date=>create( '20240101' ). " January 1, 2024

" Add time periods
lo_date->plus( iv_days = 15 ).           " January 16, 2024
lo_date->plus( iv_months = 2 ).          " March 16, 2024
lo_date->plus( iv_years = 1 ).           " March 16, 2025

" Subtract time periods
lo_date->minus( iv_days = 5 ).           " March 11, 2025
lo_date->minus( iv_months = 1 ).         " February 11, 2025

" Combined operations
lo_date->plus( 
  iv_days = 10
  iv_months = 1
  iv_years = 1
). " March 21, 2026
```

#### String Formatting
The `to_string()` method supports flexible date formatting with placeholders:

| Placeholder | Description | Example |
|-------------|-------------|---------|
| `$yyyy` | Full year | 2024 |
| `$yy` | Short year | 24 |
| `$m` | Full month name | January |
| `$mmm` | Abbreviated month | Jan |
| `$mm` | Month number | 01 |
| `$dd` | Day with leading zero | 01 |

```abap
DATA: lo_date TYPE REF TO zcl_date,
      lv_formatted TYPE string.

lo_date = zcl_date=>create( '20240315' ). " March 15, 2024

" Default format
lv_formatted = lo_date->to_string( ). " 15 March 2024

" Custom formats
lv_formatted = lo_date->to_string( '$dd/$mm/$yyyy' ).     " 15/03/2024
lv_formatted = lo_date->to_string( '$m $dd, $yyyy' ).     " March 15, 2024
lv_formatted = lo_date->to_string( '$dd-$mmm-$yy' ).     " 15-Mar-24
```

#### Date Comparison
```abap
DATA: lo_date1 TYPE REF TO zcl_date,
      lo_date2 TYPE REF TO zcl_date,
      lv_diff TYPE i.

lo_date1 = zcl_date=>create( '20240120' ). " January 20, 2024
lo_date2 = zcl_date=>create( '20240101' ). " January 1, 2024

" Using objects
lv_diff = zcl_date=>obj_diff( 
  iv_dat1 = lo_date1
  iv_dat2 = lo_date2
). " Result: 19 days

" Using dates directly
lv_diff = zcl_date=>diff( 
  iv_dats1 = '20240120'
  iv_dats2 = '20240101'
). " Result: 19 days
```

#### Month Name Utilities
```abap
DATA: lv_month TYPE string,
      lv_short TYPE string.

" Get month names by number (independent of object's date)
lv_month = lo_date->get_month_name_by_number( 3 ).      " March
lv_short = lo_date->get_short_month_name_by_number( 3 ). " Mar

" Change language for different localization
lo_date->set_language( 'D' ). " German
lv_month = lo_date->get_month_name_by_number( 3 ).      " März
```

### Best Practices
1. **Use Factory Method**: Prefer `zcl_date=>create()` over direct instantiation
2. **Language Setting**: Set language early for consistent formatting
3. **Method Chaining**: Combine operations for cleaner code
4. **Error Handling**: Always validate date inputs in production code

### Advanced Examples

#### Working with Business Dates
```abap
DATA: lo_start_date TYPE REF TO zcl_date,
      lo_end_date TYPE REF TO zcl_date,
      lv_period_text TYPE string.

" Create quarter end date
lo_start_date = zcl_date=>create( '20240101' ).
lo_end_date = zcl_date=>create( '20240101' ).
lo_end_date->plus( iv_months = 3 ).
lo_end_date->minus( iv_days = 1 ). " Last day of quarter

lv_period_text = |Q1 2024: { lo_start_date->to_string( '$m $dd' ) } - { lo_end_date->to_string( '$m $dd' ) }|.
" Result: "Q1 2024: January 01 - March 31"
```

---

## Русский

### Обзор
`ZCL_DATE` — это мощный ABAP-класс, разработанный для упрощения операций с датами и их форматирования. Он предоставляет объектно-ориентированный подход к работе с датами, предлагая удобные методы для арифметики дат, форматирования и локализации.

### Возможности
- 🗓️ **Арифметика дат**: Добавление/вычитание дней, месяцев и лет
- 🌍 **Многоязычная поддержка**: Локализованные названия месяцев
- 📅 **Гибкое форматирование**: Настраиваемый вывод даты в строковом формате
- 🔧 **Простая инициализация**: Множество вариантов конструктора
- 📊 **Сравнение дат**: Вычисление разности между датами
- 🎯 **Типобезопасность**: Строгая типизация с правильными ABAP типами

### Установка
1. Импортируйте файл класса `zcl_date.clas.abap` в вашу SAP систему
2. Активируйте класс
3. Класс готов к использованию!

### Быстрый старт

#### Базовая инициализация
```abap
DATA: lo_date TYPE REF TO zcl_date.

" Инициализация с конкретной датой
lo_date = zcl_date=>create( '20240101' ). " 1 января 2024

" Инициализация с текущей датой
lo_date = zcl_date=>create( ).

" Инициализация с пользовательским языком
lo_date = zcl_date=>create( 
  iv_date = '20240101'
  iv_language = 'R'  " Русский
).
```

### Справочник API

#### Геттеры
| Метод | Тип возврата | Описание |
|-------|--------------|----------|
| `get_day()` | `num2` | Возвращает день месяца (01-31) |
| `get_month_number()` | `num2` | Возвращает номер месяца (01-12) |
| `get_year()` | `num4` | Возвращает полный год (например, 2024) |
| `get_short_year()` | `num2` | Возвращает краткий год (например, 24) |
| `get_date()` | `dats` | Возвращает полную дату |
| `get_month_name()` | `fcltx` | Возвращает полное название месяца на текущем языке |
| `get_short_month_name()` | `fcktx` | Возвращает сокращенное название месяца |
| `get_language()` | `lang` | Возвращает текущую языковую настройку |

#### Сеттеры
| Метод | Параметр | Описание |
|-------|----------|----------|
| `set_date()` | `dats` | Устанавливает полную дату |
| `set_day()` | `num2` | Устанавливает компонент дня |
| `set_month_number()` | `num2` | Устанавливает компонент месяца |
| `set_year()` | `num4` | Устанавливает компонент года |
| `set_language()` | `lang` | Устанавливает язык для форматирования |

#### Арифметика дат
```abap
" Примеры арифметики дат
lo_date = zcl_date=>create( '20240101' ). " 1 января 2024

" Добавление периодов времени
lo_date->plus( iv_days = 15 ).           " 16 января 2024
lo_date->plus( iv_months = 2 ).          " 16 марта 2024
lo_date->plus( iv_years = 1 ).           " 16 марта 2025

" Вычитание периодов времени
lo_date->minus( iv_days = 5 ).           " 11 марта 2025
lo_date->minus( iv_months = 1 ).         " 11 февраля 2025

" Комбинированные операции
lo_date->plus( 
  iv_days = 10
  iv_months = 1
  iv_years = 1
). " 21 марта 2026
```

#### Форматирование строк
Метод `to_string()` поддерживает гибкое форматирование дат с помощью заполнителей:

| Заполнитель | Описание | Пример |
|-------------|----------|--------|
| `$yyyy` | Полный год | 2024 |
| `$yy` | Краткий год | 24 |
| `$m` | Полное название месяца | Январь |
| `$mmm` | Сокращенное название месяца | Янв |
| `$mm` | Номер месяца | 01 |
| `$dd` | День с ведущим нулем | 01 |

```abap
DATA: lo_date TYPE REF TO zcl_date,
      lv_formatted TYPE string.

lo_date = zcl_date=>create( '20240315' ). " 15 марта 2024

" Формат по умолчанию
lv_formatted = lo_date->to_string( ). " 15 Март 2024

" Пользовательские форматы
lv_formatted = lo_date->to_string( '$dd/$mm/$yyyy' ).     " 15/03/2024
lv_formatted = lo_date->to_string( '$dd $m $yyyy' ).      " 15 Март 2024
lv_formatted = lo_date->to_string( '$dd-$mmm-$yy' ).     " 15-Мар-24
```

#### Сравнение дат
```abap
DATA: lo_date1 TYPE REF TO zcl_date,
      lo_date2 TYPE REF TO zcl_date,
      lv_diff TYPE i.

lo_date1 = zcl_date=>create( '20240120' ). " 20 января 2024
lo_date2 = zcl_date=>create( '20240101' ). " 1 января 2024

" Использование объектов
lv_diff = zcl_date=>obj_diff( 
  iv_dat1 = lo_date1
  iv_dat2 = lo_date2
). " Результат: 19 дней

" Прямое использование дат
lv_diff = zcl_date=>diff( 
  iv_dats1 = '20240120'
  iv_dats2 = '20240101'
). " Результат: 19 дней
```

#### Утилиты для названий месяцев
```abap
DATA: lv_month TYPE string,
      lv_short TYPE string.

" Получение названий месяцев по номеру (независимо от даты объекта)
lv_month = lo_date->get_month_name_by_number( 3 ).      " Март
lv_short = lo_date->get_short_month_name_by_number( 3 ). " Мар

" Изменение языка для другой локализации
lo_date->set_language( 'E' ). " Английский
lv_month = lo_date->get_month_name_by_number( 3 ).      " March
```

### Лучшие практики
1. **Используйте фабричный метод**: Предпочитайте `zcl_date=>create()` прямому созданию экземпляра
2. **Настройка языка**: Устанавливайте язык заранее для согласованного форматирования
3. **Цепочка методов**: Комбинируйте операции для более чистого кода
4. **Обработка ошибок**: Всегда проверяйте входные данные дат в производственном коде

### Расширенные примеры

#### Работа с бизнес-датами
```abap
DATA: lo_start_date TYPE REF TO zcl_date,
      lo_end_date TYPE REF TO zcl_date,
      lv_period_text TYPE string.

" Создание даты окончания квартала
lo_start_date = zcl_date=>create( '20240101' ).
lo_end_date = zcl_date=>create( '20240101' ).
lo_end_date->plus( iv_months = 3 ).
lo_end_date->minus( iv_days = 1 ). " Последний день квартала

lv_period_text = |Q1 2024: { lo_start_date->to_string( '$dd $m' ) } - { lo_end_date->to_string( '$dd $m' ) }|.
" Результат: "Q1 2024: 01 Январь - 31 Март"
```

---

### License
This project is provided as-is for educational and development purposes.

### Contributing
Feel free to submit issues and enhancement requests!

### Author
Created for the ABAP development community.
