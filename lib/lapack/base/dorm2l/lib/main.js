'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dorm2l = require( './dorm2l.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorm2l, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorm2l;
