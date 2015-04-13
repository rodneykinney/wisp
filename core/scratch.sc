import com.quantifind.charts.Highcharts._

val x = (0 to 100)
line(x)
line(x,x.map(t=> t*t*t*t))

