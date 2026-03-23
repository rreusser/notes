

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dorm2r = require( './dorm2r.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorm2r, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorm2r;
