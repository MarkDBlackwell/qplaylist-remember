<?php
/* Copyright (C) 2018 Mark D. Blackwell.
    All rights reserved.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/

/* See:
https://stackoverflow.com/questions/24972424/php-create-or-write-append-in-text-file

Accept AJAX requests to append to a comments file.
*/

// First code to run:
header("content-type:application/json");

// Constants:
$comments_filename = "comments.txt";
$double_quote = "\"";
$ip_address = $_SERVER['REMOTE_ADDR'];

$my_query_keys = array(
    'comment',
    'comment_category',
    'song_artist',
    'song_category',
    'song_time',
    'song_title',
    'timestamp',
    'user_identifier'
    );

$n_dash = " - "; // Using hyphen, because it's ASCII.

$response_bad_file_json               = json_encode(array('response' => 'Unable to open comments file!'));
$response_bad_request_parameters_json = json_encode(array('response' => 'Invalid request parameters!'));
$response_ok_json                     = json_encode(array('response' => 'good'));

// See http://php.net/manual/en/function.fopen.php
// fopen with "a" means:
//   "Open for writing only; place the file pointer at the end of the file.
//   If the file does not exist, attempt to create it."

// Depends upon the above:

$myfile = fopen($comments_filename, "a") or die($response_bad_file_json);

// TODO: For security, change to use POST instead of GET.
count($my_query_keys) === count($_GET) or die($response_bad_request_parameters_json);
foreach ($my_query_keys as $key)
    isset($_GET[$key]) or die($response_bad_request_parameters_json);

$comment          = $_GET['comment'         ];
$comment_category = $_GET['comment_category'];
$song_artist      = $_GET['song_artist'     ];
$song_category    = $_GET['song_category'   ];
$song_time        = $_GET['song_time'       ];
$song_title       = $_GET['song_title'      ];
$timestamp        = $_GET['timestamp'       ];
$user_identifier  = $_GET['user_identifier' ];

// Depends upon the above:

$prefix = $timestamp . " " . $ip_address . " " . $user_identifier;

// Depends upon the above:

$string_to_write_comment =
    $prefix . " " .
    $comment_category . " " .
    $comment .
    "\n";

$string_to_write_song    =
    $prefix . " " .
    $song_category . " " .
    $song_time .
    $n_dash . $double_quote .
    $song_title .
    $double_quote . $n_dash .
    $song_artist .
    "\n";

// Depends upon the above:

fwrite($myfile, $string_to_write_song);
fwrite($myfile, $string_to_write_comment);

fclose($myfile);

echo $response_ok_json;
exit();
?>
