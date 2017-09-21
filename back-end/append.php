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

// Code to run:
header("content-type:application/json");

// Constants:
$comments_filename = "comments.txt";
$ip_address = $_SERVER['REMOTE_ADDR'];
$my_query_keys = array('comment', 'song', 'user_identifier', 'timestamp');

$response_bad_file_json               = json_encode(array('response' => 'Unable to open comments file!'));
$response_bad_request_parameters_json = json_encode(array('response' => 'Invalid request parameters!'));
$response_ok_json                     = json_encode(array('response' => 'ok'));

// Depends upon the above:

$myfile = fopen($comments_filename, "a") or die($response_bad_file_json);

// TODO: For security, change to use POST instead of GET.
count($my_query_keys) === count($_GET) or die($response_bad_request_parameters_json);
foreach ($my_query_keys as $key)
    isset($_GET[$key]) or die($response_bad_request_parameters_json);

$comment         = $_GET['comment'        ];
$song            = $_GET['song'           ];
$user_identifier = $_GET['user_identifier'];
$timestamp       = $_GET['timestamp'      ];

// Depends upon the above:

$prefix = $timestamp . " " . $ip_address . " " . $user_identifier . " ";

// Depends upon the above:

$string_to_write_comment = $prefix . $comment . "\n";
$string_to_write_song    = $prefix . $song    . "\n";

// Depends upon the above:

fwrite($myfile, $string_to_write_song);
fwrite($myfile, $string_to_write_comment);

fclose($myfile);

echo $response_ok_json;
exit();
?>
