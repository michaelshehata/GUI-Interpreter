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
                outputBox.Text = ""; // clear output box text

                string inputText = inputBox.Text;
                string interpreterReturn = Simple_Interpreter.GUIInterpret.interpret(inputText);
                outputBox.Text = interpreterReturn;
                AddToHistory(inputText, interpreterReturn); // add expression and result to history
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
    }
}