 <?php
// See https://stackoverflow.com/questions/24972424/php-create-or-write-append-in-text-file
// Copyright (c) 2017 Mark D. Blackwell
// Accept AJAX request to append to a comments file.

// Constants:
$comments_filename = "comments.txt";
$ip_address = $_SERVER['REMOTE_ADDR'];
$response_file_bad = array('response' => 'Unable to open oomments file!');
$response_request_parameter_bad = array('response' => 'Unable to access POST parameter!');
$response_ok = array('response' => 'ok');

// Depends upon whatever is above:

// Code to run:
header("content-type:application/json");

// TODO: For security, use POST instead of GET.
isset($_GET['comment']) or die(json_encode($response_request_parameter_bad));
$comment = $_GET['comment'];

isset($_GET['song']) or die(json_encode($response_request_parameter_bad));
$song = $_GET['song'];

isset($_GET['timestamp']) or die(json_encode($response_request_parameter_bad));
$timestamp = $_GET['timestamp'];

$myfile = fopen($comments_filename, "a") or die(json_encode($response_file_bad));

$string_to_write_song = $ip_address . " " . $timestamp . " " . $song . "\n";
fwrite($myfile, $string_to_write_song);

$string_to_write_comment = $ip_address . " " . $timestamp . " " . $comment . "\n";
fwrite($myfile, $string_to_write_comment);

fclose($myfile);

echo json_encode($response_ok);
exit();
?>
