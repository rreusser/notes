

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zheev = require( './zheev.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zheev, 'ndarray', ndarray );


// EXPORTS //

module.exports = zheev;
