using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CS_GUI
{
    public class CalculationHistoryItem
    {
        /**
         * Class representing a single calculation history item.
         * 
         * **/
        required public string Expression { get; set; }
        required public string Result { get; set; }

        public override string ToString()
        {
            return $"{Expression} = {Result}";
        }
    }
}
