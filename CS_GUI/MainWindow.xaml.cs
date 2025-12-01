using System.Collections.ObjectModel;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

using static API;  // or just: using API;

namespace CS_GUI
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public ObservableCollection<CalculationHistoryItem> History { get; }
            = new ObservableCollection<CalculationHistoryItem>();
        public MainWindow()
        {
            InitializeComponent();
            DataContext = this;
        }

        private void AddToHistory(string expression, string result)
        {
            History.Insert(0, new CalculationHistoryItem
            {
                Expression = expression,
                Result = result
            });
        }
        private void TextBox_TextChanged(object sender, TextChangedEventArgs e)
        {

        }

        private void buttonCalculate_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = ""; // clear error box text

                string inputText = inputBox.Text;
                // change
                API.clearPlotPoints();
                // send input text to interpreter
                string interpreterReturn = API.interpret(inputText);
                AddToHistory(inputText, interpreterReturn); // add expression and result to history
                // change
                var points = API.getPlotPoints();
                if (points.Any())
                {
                    int interpMode = API.getInterpolationMode();
                    PlotArea.Interpolation = interpMode == 0
                        ? PlottingArea.InterpolationMode.Linear
                        : PlottingArea.InterpolationMode.Spline;

                    double minX = points.Min(p => p.Item1);
                    double maxX = points.Max(p => p.Item1);
                    PlotArea.PlotFunction(points, minX, maxX);
                }
    

            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }

        }

        private void ClearHistoryButton_Click(object sender, RoutedEventArgs e)
        {
            History.Clear();
        }

        private void buttonPlot_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = "";

                string function = inputBox.Text;
                // strip leading "y ="
                string expr = function.Contains("=")
                    ? function.Split("=")[1].Trim()
                    : function.Trim();

                double minX = MinXDoubleUpDown.Value ?? 0;
                double maxX = MaxXDoubleUpDown.Value ?? 0;
                double step = StepDoubleUpDown.Value ?? 1;

                // ask interpreter to compute points
                API.plotFunction(expr, minX, maxX, step);

                // read points from F# list
                var points = API.getPlotPoints();

                // hand points over to plotting area
                PlotArea.PlotFunction(points, minX, maxX);
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }
        private string GetCurrentExpression()
        {
            // Reuse the same logic as plotting
            string text = inputBox.Text;
            return text.Contains("=")
                ? text.Split('=')[1].Trim()
                : text.Trim();
        }

        private void buttonDerivative_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = string.Empty;

                string expr = GetCurrentExpression();
                double x0 = DerivativeX0UpDown.Value ?? 0.0;
                double stepSize = 1e-5; // fixed?

                double value = API.differentiateNumeric(expr, x0, stepSize);

                AddToHistory($"f'({x0}) for {expr}", value.ToString());
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        private void buttonRoot_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = string.Empty;

                string expr = GetCurrentExpression();
                double a = RootAUpDown.Value ?? 0.0;
                double b = RootBUpDown.Value ?? 0.0;

                double tolerance = 1e-6;
                int maxIterations = 100;

                double root = API.findRootBisection(expr, a, b, tolerance, maxIterations);

                AddToHistory($"Root in [{a}, {b}] for {expr}", root.ToString());
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        private void buttonIntegrate_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = string.Empty;

                string expr = GetCurrentExpression();
                double a = IntegralAUpDown.Value ?? 0.0;
                double b = IntegralBUpDown.Value ?? 0.0;

                int steps = 1000; // fixed resolution (might change)

                double area = API.integrateTrapezoidal(expr, a, b, steps);

                AddToHistory($"∫[{a}, {b}] {expr} dx", area.ToString());
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }
        private void buttonParseTree_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = "";
                parseTreeBox.Text = "";

                string inputText = inputBox.Text;
                string tree = API.getParseTreeString(inputText);
                parseTreeBox.Text = tree;
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        private void InterpolationCombo_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            if (!IsLoaded) return;

            switch (InterpolationCombo.SelectedIndex)
            {
                case 0:
                    PlotArea.Interpolation = PlottingArea.InterpolationMode.Linear;
                    break;
                case 1:
                    PlotArea.Interpolation = PlottingArea.InterpolationMode.Spline;
                    break;
                default:
                    PlotArea.Interpolation = PlottingArea.InterpolationMode.Linear;
                    break;
            }
        }
    }
}