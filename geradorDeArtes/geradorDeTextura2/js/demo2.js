var idImagem = 1347;

var next = (function () {
	var which = 0;
	return function () {
		return {
			prev: which,
			next: which = (which + 1) % 2
		};
	};
}());

// Hook and debounce input event
function onChange($el, cb) {
	var last = $el.val();
	$el.on('input', function () {
		var val = $el.val();
		var oldVal;
		if (last !== val) {
			oldVal = last;
			last = val;
			typeof cb === 'function' && cb.call($el, val, oldVal);
		}
	});

	return {
		trigger: function () {
			typeof cb === 'function' && cb.call($el, $el.val(), last);
		}
	};
}

var fadeOptions = {
	duration: 100,
	queue: false
};

var canvas = document.createElement('canvas');
var saveButton = document.getElementById('save');
var imagem;

function prepareDownload(string, pattern) {
	if (!canvas) {
		canvas = document.createElement('canvas');
	}

	var ctx = canvas.getContext('2d');
	var img = new Image();

	img.onload = function() {
		//alert('width: '+this.width+'  Height: '+this.height);
		canvas.width = 2484;
		canvas.height = 1158;
		//ctx.drawImage(img, 0, 0);
		ctx.drawImage(img,0,0,2484,1158);

		
		
		//$("#conteudo").html("<img src='"+img+"' />");
		saveButton.download = 'Textura'+string+ '.jpg';
		try {
			saveButton.href = canvas.toDataURL('image/jpeg', 1.0);
			saveButton.click();
		} catch (err) {
			// The above is a security error in IE, so hide the save button
			saveButton.style.display = 'none';
		}
	};

	

	  imagem = canvas.toDataURL('image/jpeg', 1.0);

      imagem.replace(/^data:image\/[^;]/, 'data:application/octet-stream');  
	img.src = pattern.toDataUri();
	$(document).ready(function(){
			$("#conteudo").append("<img src='"+imagem+"' width='2484' height='1158'/>");
			$("#conteudo").append("<p><b>"+(idImagem-1)+"</b></p>");
			//$("#conteudo").append("<img src='"+img.src+"'/>");
			//<canvas id="myCanvas" width="256" height="256" style="border:1px solid #d3d3d3;">
	});

}

var changeEvent = onChange($('#string'), function (val) {
	var pattern = GeoPattern.generate(val);

	var bg = next();
	$('#bg-' + bg.next)
		.css('background-image', pattern.toDataUrl())
		.stop()
		.fadeIn(fadeOptions);
	$('#bg-' + bg.prev)
		.stop()
		.fadeOut(fadeOptions);

	prepareDownload(val, pattern);
});

// Some browsers persist field values between refresh
$(function () {	
	
	var tempo = setInterval(() => {	
	$('#string')
		.val(''+idImagem);
	changeEvent.trigger();
	idImagem++;
	}, 3000);
});
