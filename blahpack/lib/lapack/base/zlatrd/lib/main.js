

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zlatrd = require( './zlatrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlatrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlatrd;
