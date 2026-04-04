
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlangb = require( './dlangb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlangb, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlangb;
