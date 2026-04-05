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

import dtrtri = require( './index' );


// TESTS //

// The function returns a Float64Array...
{
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectType Float64Array
}

// The compiler throws an error if the function is provided a first argument which is not a string...
{
	dtrtri( 10, 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( true, 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( false, 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( null, 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( undefined, 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( [], 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( {}, 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a second argument which is not a string...
{
	dtrtri( 'no-transpose', 10, 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', true, 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', false, 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', null, 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', undefined, 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', [], 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', {}, 10, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a third argument which is not a number...
{
	dtrtri( 'no-transpose', 'no-transpose', '10', new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', true, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', false, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', null, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', undefined, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', [], new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', {}, new Float64Array( 25 ), 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fourth argument which is not a Float64Array...
{
	dtrtri( 'no-transpose', 'no-transpose', 10, '10', 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, 10, 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, true, 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, false, 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, null, 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, undefined, 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, [], 10, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, {}, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fifth argument which is not a number...
{
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), '10', 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), true, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), false, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), null, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), undefined, 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), [], 10, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), {}, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a sixth argument which is not a number...
{
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, '10', 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, true, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, false, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, null, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, undefined, 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, [], 10 ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, {}, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a seventh argument which is not a number...
{
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, 10, '10' ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, 10, true ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, 10, false ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, 10, null ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, 10, undefined ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, 10, [] ); // $ExpectError
	dtrtri( 'no-transpose', 'no-transpose', 10, new Float64Array( 25 ), 10, 10, {} ); // $ExpectError
}

// The compiler throws an error if the function is provided an unsupported number of arguments...
{
	dtrtri(); // $ExpectError
	dtrtri( 'no-transpose' ); // $ExpectError
}
