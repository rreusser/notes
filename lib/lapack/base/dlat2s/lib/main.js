
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlat2s = require( './dlat2s.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlat2s, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlat2s;
