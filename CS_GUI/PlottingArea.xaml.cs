using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using OxyPlot;
using OxyPlot.Series;
using OxyPlot.Wpf;

namespace CS_GUI
{
    /// <summary>
    /// Interaction logic for PlottingArea.xaml
    /// </summary>
    public partial class PlottingArea : UserControl
    {
        public PlottingArea()
        {
            InitializeComponent();
            PlotViewControl.Model = new PlotModel { Title = "Function Plot" };
        }

        // TODO: Add method(s) that is/are called when user clicks plot button
        //
        // *1. Check whether user has entered a valid mathematical function (e.g. `y = x^2 + 1`)
        // *2. Take in range and step parameters from GUI fields
        // 3. Loop for values of x (in accordance to range/step) and for each, call evaluation function in F#
        // 4. For each, take whatever is returned from interpreter and add it to the data series
        // 5. When done, plot
        //
        // *: 1/2 should probably be handled by MainWindow and pass to PlottingArea UserControl
       

        // TODO: rework after INT2 is implemented
        public void PlotFunction(Func<double, double> f, double xMin, double xMax, double step)
        {
            var model = new PlotModel { Title = "Function Plot" };
            var series = new LineSeries();

            for (double x = xMin; x <= xMax; x += step)
            {
                // TODO: when reworking, call the F# evaluation function in this loop
                double y = f(x);
                series.Points.Add(new DataPoint(x, y));
            }

            model.Series.Add(series);
            PlotViewControl.Model = model;
        }
    }


}
