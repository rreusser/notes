

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dorm22 = require( './dorm22.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorm22, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorm22;
