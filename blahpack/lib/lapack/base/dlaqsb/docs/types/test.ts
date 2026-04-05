/*
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

import dlaqsb = require( './index' );


// TESTS //

// The function returns a Float64Array...
{
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectType Float64Array
}

// The compiler throws an error if the function is provided a first argument which is not a string...
{
	dlaqsb( 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( true, 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( false, 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( null, 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( undefined, 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( [], 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( {}, 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a second argument which is not a number...
{
	dlaqsb( 'no-transpose', '10', 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', true, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', false, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', null, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', undefined, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', [], 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', {}, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a third argument which is not a number...
{
	dlaqsb( 'no-transpose', 10, '10', new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, true, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, false, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, null, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, undefined, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, [], new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, {}, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a fourth argument which is not a Float64Array...
{
	dlaqsb( 'no-transpose', 10, 10, '10', 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, true, 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, false, 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, null, 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, undefined, 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, [], 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, {}, 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a fifth argument which is not a number...
{
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), '10', 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), true, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), false, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), null, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), undefined, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), [], 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), {}, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a sixth argument which is not a number...
{
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, '10', 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, true, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, false, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, null, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, undefined, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, [], 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, {}, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a seventh argument which is not a number...
{
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, '10', new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, true, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, false, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, null, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, undefined, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, [], new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, {}, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a eighth argument which is not a Float64Array...
{
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, '10', 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, true, 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, false, 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, null, 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, undefined, 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, [], 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, {}, 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a ninth argument which is not a number...
{
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), '10', 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), true, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), false, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), null, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), undefined, 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), [], 10, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), {}, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a tenth argument which is not a number...
{
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, '10', 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, true, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, false, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, null, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, undefined, 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, [], 10, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, {}, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a eleventh argument which is not a number...
{
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, '10', 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, true, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, false, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, null, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, undefined, 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, [], 10, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, {}, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a twelfth argument which is not a number...
{
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, '10', 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, true, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, false, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, null, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, undefined, 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, [], 'no-transpose' ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, {}, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a thirteenth argument which is not a string...
{
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 10 ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, true ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, false ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, null ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, undefined ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, [] ); // $ExpectError
	dlaqsb( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, {} ); // $ExpectError
}

// The compiler throws an error if the function is provided an unsupported number of arguments...
{
	dlaqsb(); // $ExpectError
	dlaqsb( 'no-transpose' ); // $ExpectError
}
