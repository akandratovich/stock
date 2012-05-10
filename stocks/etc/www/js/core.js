function get_int(selector, def) {
  var c = $(selector);
  var value = parseInt(c.val());
  if (isNaN(value)) {
    c.val(def);
    return def;
  } else return value;
};

function get_float(selector, def) {
  var c = $(selector);
  var value = parseFloat(c.val());
  if (isNaN(value)) {
    c.val(def);
    return parseFloat(def);
  } else return value;
};

function get_time(selector, def) {
  var value = def;
  try {
    var date = $(selector).datetimepicker('getDate');
    value = date.getTime() - date.getTimezoneOffset() * 60 * 1000;
  } catch (e) {};
  return value;
};

function format_time(time) {
  var dt = time.getUTCDate();
  var mn = time.getUTCMonth() + 1;

  var hrs = time.getUTCHours();
  var mns = time.getUTCMinutes();
  return (dt >= 10 ? dt : '0' + dt) + '/' + (mn >= 10 ? mn : '0' + mn) + '/' + time.getUTCFullYear() + ' ' + (hrs >= 10 ? hrs : '0' + hrs) + ':' + (mns >= 10 ? mns : '0' + mns);
};

function deliver(data0, _name) {
  var data = $(data0).toArray();
  $.ajax({
    type: 'post',
    url: 'http_api.yaws',
    data: {api: "store", data: JSON.stringify(data), name: _name},
    dataType: 'text',
    success: function(back) {},
    error: function(data) {}
  });
};

function generate() {
  var count = get_int('#gcount', 10);

  var afrom = get_int('#gafrom', 100);
  if (afrom <= 0) afrom = 100;
  var ato   = get_int('#gato', 5000);

  var pfrom = get_float('#gpfrom', "0.00");
  var pto   = get_float('#gpto', "6.00");

  var now = new Date();
  var year = now.getFullYear();
  var month = now.getMonth();
  var date = now.getDate();

  var ft = new Date(year, month, date).getTime();
  var tt = new Date(year, month, date + 1).getTime();

  var tfrom = get_time('#gtfrom', ft);
  var tto   = get_time('#gtto', tt);
  var deltatime = (tto - tfrom) / count;

  var back = [];
  for (var i = 0; i < count; i++) {
    var prtime = tfrom + deltatime * i;
    var time = new Date(random_int(prtime, prtime + deltatime));

    var row_data = {
      time: time.getTime(),
      price: random(pfrom, pto),
      volume: Math.floor(random_int(afrom, ato) / 100) * 100
    };

    back.push(row_data);
  }

  return back;
};

function i_generation() {
  i_timepicker($('#gtfrom'), $('#gtto'));

  $('#save').click(function(e) {
    var _name = $('#gname').val();
    if (_name.trim().length == 0) return;
    $('#data').find('tr').not('.head').remove();

    var data0 = [];
    if ($('#data').data('fake') || $('.data0').size() == 0) data0 = generate();
    else {
      $('#data').data('fake', true);
      data0 = $('.data0').map(function() { return $(this).data('info'); });
    }

    if (data0.length == 0) return;

    while (true) {
      var end = $(data0.splice(100));
      deliver(data0, _name);
      if (end.size() == 0) return;
      else data0 = end;
    }
  });

  $('#ifshow').click(function(e) {
    if ($(this).is(':checked')) $('#generate').html("Generate and show");
    else $('#generate').html("Generate and send");
  });

  $('#generate').click(function(e) {
    var datatable = $('#data');
    datatable.data('fake', false);
    datatable.find('tr').not('.head').remove();
    datatable.show();

    $("#legend span.tip").show();

    var back = generate();
    var tdata = datatable.find('tbody');
    for (var i = 0; i < back.length; i++) {
      var row_data = back[i];
      var row = $('' +
        '<tr class="data0">' +
          '<td><span class="time0" id="t' + row_data.time + '">' + format_time(new Date(row_data.time)) + '</span></td>' +
          '<td><span class="price0">' + pf(row_data.price) + '</span></td>' +
          '<td><span class="amount0">' + row_data.volume + '</span></td>' +
        '</tr>' +
      '');

      row.data('info', row_data);
      tdata.append(row);
    }

    $('.data0 td').dblclick(function(e) {
      var thiz = e.target;
      if (thiz.tagName != "SPAN") return;

      var clazz = thiz.className;
      var value = thiz.innerHTML;
      var inp = document.createElement("input");
      inp.value = value;
      thiz.parentNode.replaceChild(inp, thiz);

      var fsave = function(e) {
        try {
          var value = inp.value;
          var span = document.createElement("span");
          span.className = clazz;
          span.innerHTML = value;
          inp.parentNode.replaceChild(span, inp);

          var row_data = $(span).parents('tr').data('info');
          row_data.time = value;
          $(span).parents('tr').data('info', row_data);
        } catch (error) {} // bug with double triggering in chrome with open console
      };

      var $inp = $(inp);
      if (clazz == "time0") {
        var tfrom = get_time('#gtfrom', Date.now());
        var tto   = get_time('#gtto', Date.now() + 1000 * 60 * 60 * 24);

        $inp.datetimepicker({
            dateFormat: 'dd/mm/yy',
            timeFormat: 'hh:mm',
            minDate: new Date(tfrom),
            maxDate: new Date(tto),
            onClose: function(dateText, inst) {
              fsave();
            }
        });
      } else {
        $inp.focusout(fsave);
        $inp.keyup(function(e) {
          if (e.keyCode == 13 || e.keyCode == 27) fsave(e);
        });
      }

      $inp.focus();
    });
  });
};

function random_int(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
};

function random(min, max) {
  return Math.random() * (max - min) + min;
};

function i_timepicker(from, to) {
  from.datetimepicker({
      dateFormat: 'dd/mm/yy',
      timeFormat: 'hh:mm',
      onClose: function(dateText, inst) {
          var endDateTextBox = to;
          if (endDateTextBox.val() != '') {
              var testStartDate = new Date(dateText);
              var testEndDate = new Date(endDateTextBox.val());
              if (testStartDate > testEndDate)
                  endDateTextBox.val(dateText);
          }
          else {
              endDateTextBox.val(dateText);
          }
      },
      onSelect: function (selectedDateTime){
          var start = $(this).datetimepicker('getDate');
          to.datetimepicker('option', 'minDate', new Date(start.getTime()));
      }
  });

  to.datetimepicker({
      dateFormat: 'dd/mm/yy',
      timeFormat: 'hh:mm',
      onClose: function(dateText, inst) {
          var startDateTextBox = from;
          if (startDateTextBox.val() != '') {
              var testStartDate = new Date(startDateTextBox.val());
              var testEndDate = new Date(dateText);
              if (testStartDate > testEndDate)
                  startDateTextBox.val(dateText);
          }
          else {
              startDateTextBox.val(dateText);
          }
      },
      onSelect: function (selectedDateTime){
          var end = $(this).datetimepicker('getDate');
          from.datetimepicker('option', 'maxDate', new Date(end.getTime()));
      }
  });

  var now = new Date();
  var year = now.getFullYear();
  var month = now.getMonth();
  var date = now.getDate();

  var ft = new Date(year, month, date);
  var tt = new Date(year, month, date + 1);

  from.datetimepicker('option', 'maxDate', tt);
  to.datetimepicker('option', 'minDate', ft);

  from.datetimepicker('setDate', ft);
  to.datetimepicker('setDate', tt);
};

function i_bind() {
  $('#find').click(function(e) {
    var _from = get_time('#from', 0);
    var _to = get_time('#to', 0);
    var _name = $('#name').val();
    var _scale = $('#scale').val();

    if (_name.trim().length == 0) return;

    var time0 = Date.now();
    $.ajax({
      type: 'get',
      url: 'http_api.yaws',
      data: {api: "search", name: _name, from: _from, to: _to, scale: _scale},
      dataType: 'json',
      success: function(back) {
        var datatable = $('#result');
        datatable.find('tr').not('.head').remove();
        datatable.show();

        var tdata = datatable.find('tbody');
        for (var i in back) {
          var obj = back[i];
          var time = new Date(obj.from);
          var begin = obj.begin;
          var end = obj.end;
          var min = obj.min;
          var max = obj.max;
          var total = obj.summary;

          tdata.append('' +
            '<tr>' +
              '<td><span>' + format_time(time) + '</span></td>' +
              '<td><span>' + pf(begin) + '</span></td>' +
              '<td><span>' + pf(end) + '</span></td>' +
              '<td><span>' + pf(min) + '</span></td>' +
              '<td><span>' + pf(max) + '</span></td>' +
              '<td><span>' + total + '</span></td>' +
            '</tr>' +
          '');
        }

        var stock = $('#stock');
        if (back.length > 10) {
          stock.find('#stockdiv').remove();
          var graph = $('<div id="stockdiv"></div>');
          stock.append(graph);
          graph.hide();
          graph.width(720);
          stock.show();
          stock.find('#showg').unbind();
          draw_stock('stockdiv', back, $("#name").val().toUpperCase());
          stock.find('#showg').click(function(e) { graph.toggle('scale'); });
        } else stock.hide();
      },
      error: function(back) {
        console.log(back);
      }
    });
  });
};

function pfs(n) {
  if (n < 1024) return n + ' B';
  if (n < 1024 * 1024) return pf(n / 1024) + ' KB';
  if (n < 1024 * 1024 * 1024) return pf(n / (1024 * 1024)) + ' MB';
  return pf(n / (1024 * 1024 * 1024)) + ' GB'; // lolwut ?
};

function pf(n) {
  var sn = String(n);
  var pp = sn.search("\\.");
  if (pp == -1) {
    return sn + ".00";
  } else {
    if (sn.length - 3 >= pp) return sn.substring(0, pp + 3);
    return (sn + "00").substring(0, pp + 3);
  }
};

// http://stackoverflow.com/questions/901115/get-query-string-values-in-javascript
function query_param(name) {
  name = name.replace(/[\[]/, "\\\[").replace(/[\]]/, "\\\]");
  var regexS = "[\\?&]" + name + "=([^&#]*)";
  var regex = new RegExp(regexS);
  var results = regex.exec(window.location.search);
  if(results == null) return "";
  else return decodeURIComponent(results[1].replace(/\+/g, " "));
};

function i_info() {
  if (typeof WebSocket != "undefined") {
    var cpu_chart = undefined;
    var mem_chart = undefined;
    ws_perf(function(msg) {
      var data = JSON.parse(msg.data);
      if (typeof data.now == "undefined") return;

      var mincount = 40;
      if (cpu_chart.series[0].data.length < mincount) {
        for (var i = 0; i < mincount; i++) {
          cpu_chart.series[0].addPoint([data.now - 2000 * (mincount + 1 - i), data.cpu], false, false);
          mem_chart.series[0].addPoint([data.now - 2000 * (mincount + 1 - i), data.mem / (1024 * 1024)], false, false);
        }
      }

      cpu_chart.series[0].addPoint([data.now, data.cpu], true, true);
      mem_chart.series[0].addPoint([data.now, data.mem / (1024 * 1024)], true, true);
    }, function() {
      $('.perf').show();
      cpu_chart = draw_perf('cpu', 'CPU (System)', '%');
      mem_chart = draw_perf('mem', 'Memory (Erlang Node)', 'mb');
    });
  }

  $.ajax({
    type: 'get',
    url: 'http_api.yaws',
    data: {api: "names"},
    dataType: 'json',
    success: function(back) {
      var rtable = $('#info tbody');

      for (var i in back) {
        var obj = back[i];

        rtable.append('' +
          '<tr>' +
            '<td><span class="ninfo">' + obj + '</span></td>' +
            // '<td><a href="#" class="hinfo"><span> >> </span></a></td>' +
          '</tr>' +
        '');
      }

      var check_value = function() {
        var value = $('#sfilter').val();
        var all = $('#info tr');
        var ignore = all.filter(function(i) {
          return $(this).find('.ninfo').text().search(value.toUpperCase()) == -1;
        });

        all.show();
        ignore.hide();
      };

      var cid = -1;
      $('#sfilter').focus(function(e) {
        cid = setInterval(check_value, 500);
      });

      $('#sfilter').focusout(function(e) {
        check_value();
        clearInterval(cid);
      });

      $('.ninfo').click(function(e) {
        var _name = $(this).text();
        // var _name = $(this).parent().prev().find('span').text();
        $.ajax({
          type: 'get',
          url: 'http_api.yaws',
          data: {api: "info", name: _name},
          dataType: 'json',
          success: function(back) {
            if (back.length == 0) return;
            var infotable = $('#infor tbody');

            infotable.parent().show();
            infotable.html("");
            infotable.append('' +
              '<tr>' +
                '<td><span>Name:</span></td>' +
                '<td><span>' + (back.name || '') + '</span></td>' +
              '</tr>' +
              '<tr>' +
                '<td><span>Heap size:</span></td>' +
                '<td><span>' + (pfs(back.heap_size * back.wordsize) || '') + '</span></td>' +
              '</tr>' +
              '<tr>' +
                '<td><span>Total heap size:</span></td>' +
                '<td><span>' + (pfs(back.total_heap_size * back.wordsize) || '') + '</span></td>' +
              '</tr>' +
              '<tr>' +
                '<td></td>' +
                '<td><a href="index.yaws?name=' + _name + '"><span>go to search</span></a></td>' +
              '</tr>' +
              '<tr>' +
                '<td></td>' +
                '<td>' +
                  '<input style="float: left;" type="checkbox" name="erase"/>' +
                  '<a href="javascript:erase(\'' + _name + '\')"><span>delete</span></a>' +
                '</td>' +
              '</tr>' +
            '');

            infotable.find('td:last-child').css('text-align', 'right');
          },
          error: function(back) {
            console.log(back);
          }
        });
      });
    },
    error: function(back) {
      console.log(back);
    }
  });
};

function erase(_name) {
  if ($('[name=erase]').is(':checked')) {
    $.ajax({
      type: 'post',
      url: 'http_api.yaws',
      data: {api: "erase", name: _name},
      dataType: 'text',
      success: function(back) {
        $('#info tr').filter(function(e) { return $(this).find('.ninfo').text() == _name; }).remove();
        $('#infor').find('tbody').html("");
      },
      error: function(data) {}
    });
  }
};
