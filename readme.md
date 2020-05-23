# Класс для удобного использования даты в abap

## Инициализация:
```abap
data: lo_date type zcl_date.

lo_date = zcl_date=>create( '20200101' ). " 1 января 2020
" or
lo_date = zcl_date=>create( ). " текущая дата
" or
lo_date = zcl_date=>create( 
    iv_date = '20200101'
    iv_language = 'E'
). " инициализация с кастомным языком (default = sy-langu)
```

## Геттеры и сеттеры

1. **get**
   1. **get_day** -- получить номер дня в месяце *(num2)*
   2. **get_month_number** -- получить номер месяца *(num2)*
   3. **get_year** -- получить год *(num4)*
   4. **get_language** -- получить текущий язык *(lang)*
   5. **get_date** -- получить дату *(dats)*
   6. **get_month_name** -- получить название месяца на текущем языке *(fcltx)*
   7. **get_short_month_name** -- получить краткое название месяца на текущем языке *(янв, фев...)* *(fcktx)*
2. **set**
   1. **set_date** -- установить дату *(dats)*
   2. **set_day** -- установить день *(num2)*
   3. **set_year** -- установить год *(num4)*
   4. **set_month_number** -- установить месяц *(num2)*
   5. **set_language** -- установить язык *(lang)*

### Пример:
```abap

data: lo_date  type zcl_date,
      lv_dats  type dats,
      lv_month type string.

lo_date = zcl_date=>create( ).
lo_date->set_year( 2001 ).
lo_date->set_month_number( 9 ).
lo_date->set_day( 11 ).

lv_dats = lo_date->get_date( ).
lv_month = lo_date->get_month_name( ).

write lv_dats. " 11 сентября 2001
write lv_month. " Сентябрь

```

## Методы инстанса

1. get_short_month_name_by_number
2. get_month_name_by_number

*(Получить имя месяца/короткое имя месяца по его номеру)*

```abap

data: lv_month type string,
      lv_short type string.

" Эти методы никак не взаимодействуют с датой, которая хранится внутри объекта. 
" Лишь возвращают названия месяцев, переданных в аргументах 

lv_short = lo_date->get_short_month_name_by_number( 1 ). 
lv_month = lo_date->get_month_name_by_number( 1 ).

write lv_short. " Янв
write lv_month. " Январь

" Аналогично, с другими языками
lo_date->set_language( 'E' ).

lv_short = lo_date->get_short_month_name_by_number( 1 ). 
lv_month = lo_date->get_month_name_by_number( 1 ).

write lv_short. " Jan
write lv_month. " January

```

3. minus
4. plus

*(Добавить/убавить день/месяц/год)*

```abap

lo_date = zcl_date=>create( '01012020' ). " 01.01.2020

lo_date->minus( iv_years = 2 ). " 01.01.2018
lo_date->plus( iv_days = 3 ).   "04.01.2018
lo_date->plus( iv_months = 1 ). "04.02.2018

" Несколько аргументов
lo_date->plus( 
    iv_days  = 1
    iv_years = 2
). " 05.02.2020

```

5. to_string
*(Конвертировать дату в человеко-читаемую строку)

По умолчанию: в формате 01 Января 2020 ($dd $m $yyyy)*

Принимает один (опциональный) аргумент -- маску вывода
**Доступные ключевые слова в маске:**

1. **$yyyy** -- длинное представление года (2019)
2. **$yy** -- короткое представление года (19)
3. **$m** -- название месяца на текущем языке (Январь)
4. **$mmm** -- короткое название месяца на текущем языке (Янв)
5. **$mm** -- номер месяца (01)
6. **$dd** -- день

```abap

data: lo_date     type zcl_date,
      lv_formated type string.

lo_date = zcl_date=>create( '01012020' ). " 01.01.2020
lv_formated = lo_date->to_string( 'Месяц: $m Год: $yyyy День: $dd' ).

write lv_rormated. " Месяц: Январь Год: 2020 День: 01

```

## Статические методы
1. obj_diff
2. diff

Возвращает разницу в днях между двумя объектами dats(diff) / zcl_date (obj_diff)

```abap

data: lo_date1 type zcl_date,
      lo_date2 type zcl_date,
      lv_dats1 type dats,
      lv_dats2 type dats,
      lv_diff_dats type i,
      lv_diff_zcl_d type i.

lv_dats1 = '01012020'.
lv_dats2 = '20012020'.

lo_date1 = zcl_date=>create( lv_dats1 ).
lo_date2 = zcl_date=>create( lv_dats2 ).


" Разница в датах между двумя переменными dats
lv_diff_dats = zcl_date=>diff( iv_dats1 = lv_dats1
                               iv_dats2 = lv_dats2 ).

" Разница в датах между двумя объектами zcl_date
lv_diff_zcl_d = zcl_date=>obj_diff( iv_dat1 = lo_date1
                                    iv_dat2 = lo_date2 ).

write lv_diff_dats.  " 19
write lv_diff_zcl_d. " 19

```
