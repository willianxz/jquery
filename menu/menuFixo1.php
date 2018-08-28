<!DOCTYPE html>
<!--
To change this license header, choose License Headers in Project Properties.
To change this template file, choose Tools | Templates
and open the template in the editor.
-->
<html>
    <head>
        <meta charset="UTF-8">
        <title></title>
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"></script>
        <style>
            .nav-container{ background: url('images/menu1.png') repeat-x 0 0;}
                .f-nav{ z-index: 9999; position: fixed; left: 0; top: 0; width: 100%;} /* this make our menu fixed top */

            .nav { height: 42px;}
                .nav ul { list-style: none; }
                .nav ul li{float: left; margin-top: 6px; padding: 6px; border-right: 1px solid #ACACAC;}
                .nav ul li:first-child{ padding-left: 0;}
                .nav ul li a { }
                .nav ul li a:hover{ text-decoration: underline;}
        </style>
        
        
    </head>
    <body>
        
        
        
         <div style="width: 100%;height: 1600px;background: #00BFFF;">
             <div class="nav-container" style="width: 100%;">
            <div class="nav">
                <ul>
                    <li><a href="">Home</a></li>
                    <li><a href="">CSS</a></li>
                    <li><a href="">PHP</a></li>
                    <li><a href="">SEO</a></li>
                    <li><a href="">jQuery</a></li>
                    <li><a href="">Wordpress</a></li>
                    <li><a href="">Services</a></li>
                </ul>
                <div class="clear"></div> /*clear floating div*/
            </div>
            </div>
            
         </div>
        <script>
            jQuery("document").ready(function($){

                var nav = $('.nav-container');

                $(window).scroll(function () {
                    if ($(this).scrollTop() > 136) {
                        nav.addClass("f-nav");
                    } else {
                        nav.removeClass("f-nav");
                    }
                });

            });

        </script>
        
    </body>
</html>
