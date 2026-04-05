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

import zlaset = require( './index' );


// TESTS //

// The function returns a Float64Array...
{
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectType Float64Array
}

// The compiler throws an error if the function is provided a first argument which is not a string...
{
	zlaset( 10, 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( true, 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( false, 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( null, 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( undefined, 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( [], 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( {}, 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a second argument which is not a number...
{
	zlaset( 'no-transpose', '10', 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', true, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', false, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', null, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', undefined, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', [], 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', {}, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a third argument which is not a number...
{
	zlaset( 'no-transpose', 10, '10', 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, true, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, false, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, null, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, undefined, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, [], 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, {}, 1.0, 1.0, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a sixth argument which is not a Float64Array...
{
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, '10', 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, 10, 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, true, 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, false, 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, null, 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, undefined, 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, [], 10, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, {}, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a seventh argument which is not a number...
{
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), '10', 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), true, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), false, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), null, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), undefined, 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), [], 10, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), {}, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a eighth argument which is not a number...
{
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, '10', 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, true, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, false, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, null, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, undefined, 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, [], 10 ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, {}, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a ninth argument which is not a number...
{
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, '10' ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, true ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, false ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, null ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, undefined ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, [] ); // $ExpectError
	zlaset( 'no-transpose', 10, 10, 1.0, 1.0, new Float64Array( 25 ), 10, 10, {} ); // $ExpectError
}

// The compiler throws an error if the function is provided an unsupported number of arguments...
{
	zlaset(); // $ExpectError
	zlaset( 'no-transpose' ); // $ExpectError
}
