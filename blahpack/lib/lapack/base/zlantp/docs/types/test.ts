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

import zlantp = require( './index' );


// TESTS //

// The function returns a number...
{
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectType number
}

// The compiler throws an error if the function is provided a first argument which is not a string...
{
	zlantp( 10, 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( true, 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( false, 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( null, 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( undefined, 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( [], 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( {}, 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
}

// The compiler throws an error if the function is provided a second argument which is not a string...
{
	zlantp( 'no-transpose', 10, 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', true, 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', false, 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', null, 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', undefined, 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', [], 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', {}, 'no-transpose', 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
}

// The compiler throws an error if the function is provided a third argument which is not a string...
{
	zlantp( 'no-transpose', 'no-transpose', 10, 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', true, 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', false, 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', null, 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', undefined, 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', [], 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', {}, 10, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
}

// The compiler throws an error if the function is provided a fourth argument which is not a number...
{
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', '10', new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', true, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', false, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', null, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', undefined, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', [], new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', {}, new Float64Array( 50 ), new Float64Array( 25 ) ); // $ExpectError
}

// The compiler throws an error if the function is provided a fifth argument which is not a Complex128Array...
{
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, '10', new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, 10, new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, true, new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, false, new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, null, new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, undefined, new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, [], new Float64Array( 25 ) ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, {}, new Float64Array( 25 ) ); // $ExpectError
}

// The compiler throws an error if the function is provided a sixth argument which is not a Float64Array...
{
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), '10' ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), true ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), false ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), null ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), undefined ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), [] ); // $ExpectError
	zlantp( 'no-transpose', 'no-transpose', 'no-transpose', 10, new Float64Array( 50 ), {} ); // $ExpectError
}

// The compiler throws an error if the function is provided an unsupported number of arguments...
{
	zlantp(); // $ExpectError
	zlantp( 'no-transpose' ); // $ExpectError
}
