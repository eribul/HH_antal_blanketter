function Stapeldiagram(render, titel, subtitel, kat, xaxis, yaxis, ser) {
     var chart = new Highcharts.Chart({
           chart: {   
             type: 'column',
             renderTo : render, 
             marginBottom: 100, 
             height: 459.5, 
             width: 450, 
             style: { 
             fontSize: '8px'} 
           }, 
           credits: false,   
           title: {   
               text: titel, 
                style: { 
               fontSize: '12px'} 
           }, 
           subtitle: { 
            text: subtitel, 
            style: { 
                 fontSize: '10px'} 
          },   
           xAxis: {   
               categories: kat,  
                title: {  
                text: xaxis}, 
                 labels: { 
                 style: { 
                     fontSize:'10px'} 
                   }   
           },   
           yAxis: {   
               min: 0, 
               gridLineColor: '#EFEFEF',   
               title: {   
                   text: yaxis},   
               stackLabels: {   
                   enabled: true,   
                   style: {   
                       fontWeight: 'bold',   
                       color: (Highcharts.theme && Highcharts.theme.textColor) || 'gray', 
                       fontSize: '10px'}   
               } 
           },   
           legend: {   
               align: 'right',   
               x: 0,   
               verticalAlign: 'bottom',   
               y: -10,   
               floating: true,   
               backgroundColor: (Highcharts.theme && Highcharts.theme.background2) || 'white',   
               borderColor: '#CCC',   
               borderWidth: 1,   
               shadow: false, 
               symbolHeight: 8, 
               itemStyle: { 
                  fontSize:'8px'}   
           },
          lang: {
              noData: "Det finns inga patienter för rapporten/mallen <br> för givna parametrar, var god försök igen med andra <br> parameterval. Om problemet kvarstår var god <br> kontakta ansvarig nationell statistiker"
          },
            noData: {
              style: {
                fontWeight: 'bold',
                fontSize: '12px',
                color: '#999999'
            }
          }, 
           exporting: { 
           enabled: false,  
            sourceHeight: 1024,  
            sourceWidth: 1024  
           },     
           tooltip: {   
             pointFormat: '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>', 
             shared: true  
           },   
           plotOptions: {   
               column: {   
                   stacking: 'normal',   
                   dataLabels: {   
                       enabled: true,   
                       color: (Highcharts.theme && Highcharts.theme.dataLabelsColor) || 'white',   
                       style: {   
                           textShadow: '0 0 3px black', 
                           fontSize: '8px'}   
                   }   
               }   
           },   
          series: ser 
         });   
   }; 