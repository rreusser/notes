'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zspr = require( './zspr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zspr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zspr;
