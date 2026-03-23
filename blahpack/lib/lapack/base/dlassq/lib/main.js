

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlassq = require( './dlassq.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlassq, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlassq;
