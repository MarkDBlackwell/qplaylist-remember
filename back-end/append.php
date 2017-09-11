<?php
/* Copyright (C) 2017 Mark D. Blackwell.
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see http://www.gnu.org/licenses/ .

See https://stackoverflow.com/questions/24972424/php-create-or-write-append-in-text-file

Accept AJAX request to append to a comments file.
*/

// Constants:
$response_ok                    = array('response' => 'ok');
$response_request_parameter_bad = array('response' => 'Unable to access POST parameter!');
$response_file_bad              = array('response' => 'Unable to open oomments file!');

$ip_address = $_SERVER['REMOTE_ADDR'];
$comments_filename = "comments.txt";

// Depends upon the above:

// Code to run:
header("content-type:application/json");

$myfile = fopen($comments_filename, "a") or die(json_encode($response_file_bad));

// TODO: For security, change to use POST instead of GET.
isset(       $_GET['timestamp']) or die(json_encode($response_request_parameter_bad));
$timestamp = $_GET['timestamp'];

isset       ($_GET['comment'  ]) or die(json_encode($response_request_parameter_bad));
$comment   = $_GET['comment'  ];

isset       ($_GET['song'     ]) or die(json_encode($response_request_parameter_bad));
$song      = $_GET['song'     ];

// Depends upon the above:

$prefix = $timestamp . " " . $ip_address . " ";

$string_to_write_comment = $prefix . $comment . "\n";
$string_to_write_song    = $prefix . $song    . "\n";

fwrite($myfile, $string_to_write_song);
fwrite($myfile, $string_to_write_comment);

fclose($myfile);

echo json_encode($response_ok);
exit();
?>
