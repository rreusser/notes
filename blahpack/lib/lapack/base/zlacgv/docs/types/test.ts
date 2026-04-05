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

import zlacgv = require( './index' );


// TESTS //

// The function returns a Float64Array...
{
	zlacgv( 10, new Float64Array( 25 ), 10, 10 ); // $ExpectType Float64Array
}

// The compiler throws an error if the function is provided a first argument which is not a number...
{
	zlacgv( '10', new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zlacgv( true, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zlacgv( false, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zlacgv( null, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zlacgv( undefined, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zlacgv( [], new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zlacgv( {}, new Float64Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a second argument which is not a Float64Array...
{
	zlacgv( 10, '10', 10, 10 ); // $ExpectError
	zlacgv( 10, 10, 10, 10 ); // $ExpectError
	zlacgv( 10, true, 10, 10 ); // $ExpectError
	zlacgv( 10, false, 10, 10 ); // $ExpectError
	zlacgv( 10, null, 10, 10 ); // $ExpectError
	zlacgv( 10, undefined, 10, 10 ); // $ExpectError
	zlacgv( 10, [], 10, 10 ); // $ExpectError
	zlacgv( 10, {}, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a third argument which is not a number...
{
	zlacgv( 10, new Float64Array( 25 ), '10', 10 ); // $ExpectError
	zlacgv( 10, new Float64Array( 25 ), true, 10 ); // $ExpectError
	zlacgv( 10, new Float64Array( 25 ), false, 10 ); // $ExpectError
	zlacgv( 10, new Float64Array( 25 ), null, 10 ); // $ExpectError
	zlacgv( 10, new Float64Array( 25 ), undefined, 10 ); // $ExpectError
	zlacgv( 10, new Float64Array( 25 ), [], 10 ); // $ExpectError
	zlacgv( 10, new Float64Array( 25 ), {}, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fourth argument which is not a number...
{
	zlacgv( 10, new Float64Array( 25 ), 10, '10' ); // $ExpectError
	zlacgv( 10, new Float64Array( 25 ), 10, true ); // $ExpectError
	zlacgv( 10, new Float64Array( 25 ), 10, false ); // $ExpectError
	zlacgv( 10, new Float64Array( 25 ), 10, null ); // $ExpectError
	zlacgv( 10, new Float64Array( 25 ), 10, undefined ); // $ExpectError
	zlacgv( 10, new Float64Array( 25 ), 10, [] ); // $ExpectError
	zlacgv( 10, new Float64Array( 25 ), 10, {} ); // $ExpectError
}

// The compiler throws an error if the function is provided an unsupported number of arguments...
{
	zlacgv(); // $ExpectError
	zlacgv( 10 ); // $ExpectError
}
